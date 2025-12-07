package core

import cats.data.EitherT
import cats.data.OptionT
import cats.data.State
import cats.data.StateT
import cats.implicits.*
import core.Bindings.*
import core.Context.*
import core.Shifting.*
import core.Terms.*
import core.Types.*
import parser.FuseParser.*
import parser.Info.*
import parser.Info.ShowInfo.*

import scala.annotation.tailrec
import scala.util.*
import fuse.Utils.*
import fuse.SpecializedMethodUtils
import core.Instantiations.Instantiation

object TypeChecker {
  val MainFunction = "main"

  def run(binds: List[Bind]): Either[Error, List[Bind]] =
    checkBindings(binds).value.runEmptyA.value

  def checkBindings(
      binds: List[Bind]
  ): StateEither[List[Bind]] =
    binds
      .traverse(bind =>
        for {
          (binding, insts) <- checkBinding(bind.b)
          _ <- checkTypeInstanceMethodBinding(bind.i, binding)
          _ <- checkTypeInstance(bind.i, binding)
          id <- EitherT.liftF(addBinding(bind.i, binding))
        } yield Bind(id, binding, insts)
      )
      .flatMap(binds =>
        binds.exists { bind => bind.i == MainFunction } match {
          case true  => binds.pure[StateEither]
          case false => TypeError.format(MainFunctionNotFoundTypeError())
        }
      )

  def checkBinding(
      b: Binding
  ): StateEither[(Binding, List[Instantiation])] = b match {
    case NameBind              => EitherT.rightT((NameBind, Nil))
    case t @ TypeVarBind(_, _) => EitherT.rightT((t, Nil))
    case v @ VarBind(_)        => EitherT.rightT((v, Nil))
    case TypeAbbBind(ty, None) =>
      pureKindOf(ty).map(k => (TypeAbbBind(ty, Some(k)), Nil))
    case TermAbbBind(term, None) =>
      pureInfer(term).map((ty, insts) => (TermAbbBind(term, Some(ty)), insts))
    case _ => EitherT.rightT(b, Nil)
  }

  def pureInfer(t: Term)(implicit
      checking: Boolean = true
  ): StateEither[(Type, List[Instantiation])] = for {
    m <- EitherT.liftF(addMark("p"))
    (iT, insts) <- infer(t)
    aT <- apply(iT)(shift = false)
    aI <- insts.traverse(applyInst(_)(shift = false))
    _ <- EitherT.liftF(peel(m))
  } yield (aT, aI)

  /** Infers a type for `exp` with input context `ctx`.
    * @return
    *   the inferred type and the output context.
    */
  def infer(exp: Term)(implicit
      checking: Boolean = true
  ): StateEither[(Type, List[Instantiation])] =
    exp match {
      case TermAscribe(info, t1, typeToAscribe) =>
        for {
          _ <- checkKindStar(typeToAscribe)
          (tyT1, insts) <- pureInfer(t1)
          t <- isSubtypeWithTypeError(
            tyT1,
            typeToAscribe,
            AscribeWrongTypeError(
              info,
              tyT1,
              typeToAscribe
            )
          )
        } yield (t, insts)
      case TermVar(info, idx, _) => Context.getType(info, idx).map((_, Nil))
      case TermClosure(info, variable, Some(variableType), expr) =>
        for {
          _ <- checkKindStar(variableType)
          _ <- EitherT.liftF(
            Context.addBinding(variable, VarBind(variableType))
          )
          (tyT2, insts) <- pureInfer(expr)
        } yield (TypeArrow(info, variableType, typeShift(-1, tyT2)), insts)
      case TermClosure(info, variable, None, expr) =>
        for {
          eA <- EitherT.liftF(addEVar("a", info, TypeEFreeBind))
          eC <- EitherT.liftF(addEVar("c", info, TypeEFreeBind))
          b <- EitherT.liftF(addBinding(variable, VarBind(eA)))
          (insts, sol1) <- check(expr, eC)
          eAS <- apply(eA)(shift = true)
          _ <- EitherT.liftF(peel(b))
          eAS1 <- EitherT.liftF(typeShiftOnContextDiff(eAS))
          eCS <- apply(eC)(shift = true)
          aI <- insts.traverse(applyInst(_)(shift = true))
          // Build closure instantiation to have its parameter type
          // available in monomorphization and code-gen phases.
          // Resolve type constructors to TypeIds while context is valid
          resolvedType <- EitherT.liftF(State.inspect { (ctx: Context) =>
            resolveTypeConstructors(eAS1, ctx)
          })
          closureInst = Instantiation(
            i = variable, // Use variable name as identifier
            term = TermClosure(info, variable, None, expr),
            tys =
              List(resolvedType), // The resolved parameter type with TypeIds
            cls = List(),
            r = Some(
              -1
            ) // Special marker: -1 indicates closure param instantiation
          )
        } yield (TypeArrow(info, eAS1, typeShift(-1, eCS)), closureInst :: aI)
      case TermAbs(info, variable, variableType, expr, returnType) =>
        for {
          _ <- checkKindStar(variableType)
          _ <- EitherT.liftF(
            Context.addBinding(variable, VarBind(variableType))
          )
          (exprType, insts) <- pureInfer(expr)
          shiftedExprType <- EitherT.liftF(typeShiftOnContextDiff(exprType))
          _ <- returnType match {
            case Some(ty) =>
              isSubtypeWithTypeError(
                ty,
                shiftedExprType,
                WrongReturnTypeError(ty.info, shiftedExprType, ty)
              )
            case _ => ().pure[StateEither]
          }
        } yield (
          TypeArrow(info, variableType, typeShift(-1, shiftedExprType)),
          insts
        )
      case TermApp(info, fun, arg) =>
        for {
          (funType, insts) <- infer(fun)
          simplifiedFun <- EitherT.liftF(simplifyType(funType))
          (ty, (appInsts, solutions)) <- inferApp(simplifiedFun, arg)
          buildInsts <- Instantiations.build(fun, solutions, insts ::: appInsts)
        } yield (ty, buildInsts)
      case TermProj(info, ty, label) =>
        for {
          (tyT1, insts) <- pureInfer(ty)
          tyT1S <- EitherT.liftF(simplifyType(tyT1))
          fieldType <- tyT1S match {
            case TypeRec(_, _, _, TypeRecord(_, fields)) =>
              fields
                .find { case (f, _) => f == label }
                .map { case (_, ty) => typeShift(-1, ty).pure[StateEither] }
                .getOrElse(
                  TypeError.format(
                    FieldNotFoundTypeError(info, tyT1, label)
                  )
                )
            case _ => TypeError.format(NoFieldsOnTypeError(info, tyT1))
          }
        } yield (fieldType, insts)
      case TermMethodProj(info, term, method) =>
        for {
          (tyT1, insts) <- pureInfer(term)
          tyT1S <- EitherT.liftF(simplifyType(tyT1))
          rootTypeVarOption = findRootTypeVar(tyT1)
          typeBounds <- EitherT.liftF(
            getTypeBounds(rootTypeVarOption.getOrElse(tyT1S))
          )
          // If method is already specialized, extract the base method name
          baseMethod = SpecializedMethodUtils.extractBaseMethodName(method)
          methodType <- inferMethod(tyT1, tyT1S, typeBounds, baseMethod, info)
        } yield (methodType, insts)
      case TermAssocProj(info, ty, method) =>
        for {
          tyS <- EitherT.liftF(simplifyType(ty))
          rootTypeVarOption = findRootTypeVar(ty)
          typeBounds <- EitherT.liftF(
            getTypeBounds(rootTypeVarOption.getOrElse(tyS))
          )
          assocMethodType <- inferMethod(ty, tyS, typeBounds, method, info)
        } yield (assocMethodType, Nil)
      case TermFix(info, t1) =>
        for {
          (tyT1, insts) <- infer(t1)
          tyT1S <- EitherT.liftF(simplifyType(tyT1))
          bodyType <- tyT1S match {
            case TypeArrow(_, tyT11, tyT12) => tyT12.pure[StateEither]
            case _                          =>
              TypeError.format(InvalidFunctionTypeError(info, tyT1S))
          }
        } yield (bodyType, insts)
      case TermRecord(info, fields) =>
        fields
          .traverse { case (v, term) =>
            pureInfer(term).map { case (t, insts) => ((v, t), insts) }
          }
          .map(v =>
            val (r, insts) = v.unzip
            (TypeRecord(info, r), insts.flatten)
          )
      case TermFold(info, tyS) =>
        EitherT
          .liftF(simplifyType(tyS))
          .flatMap(_ match {
            case TypeRec(_, _, _, tyT) =>
              (
                TypeArrow(info, typeSubstituteTop(tyS, tyT), tyS): Type,
                Nil
              ).pure
            case _ =>
              TypeError.format(InvalidFoldForRecursiveTypeError(info, tyS))
          })
      case TermLet(_, variable, t1, t2) =>
        for {
          (tyT1, sol1) <- infer(t1)
          _ <- EitherT.liftF(addBinding(variable, VarBind(tyT1)))
          (tyT2, sol2) <- pureInfer(t2)
        } yield (typeShift(-1, tyT2), sol1 ::: sol2)
      case TermTAbs(info, v, cls, t) =>
        for {
          typeClassKinds <- cls.traverse(getTypeClassKind(_))
          kind = typeClassKinds.headOption.getOrElse(KindStar)
          _ <- EitherT.liftF(addBinding(v, TypeVarBind(kind, cls)))
          _ <- typeClassKinds.forall(_ == kind) match {
            case false =>
              TypeError.format(KindTypeClassMismatchTypeError(info, cls))
            case true => ().pure[StateEither]
          }
          ty <- pureInfer(t).map { case (t, insts) =>
            (TypeAll(info, v, kind, cls, t), insts)
          }
        } yield ty
      case TermTApp(info, expr, ty2) =>
        for {
          k2 <- kindOf(ty2)
          (ty1, insts) <- pureInfer(expr)
          tyT1 <- EitherT.liftF(simplifyType(ty1))
          ty <- tyT1 match {
            case TypeAll(_, _, k1, _, tyT2) =>
              typeErrorEitherCond(
                k1 == k2,
                typeSubstituteTop(ty2, tyT2),
                InvalidTypeArgumentTypeError(ty2.info, ty2)
              )
            case _ =>
              TypeError.format(
                TypeArgumentsNotAllowedTypeError(
                  ty2.info,
                  tyT1
                )
              )
          }
        } yield (ty, insts)
      case TermMatch(info, exprTerm, cases) =>
        for {
          // Get the type of the expression to match.
          (exprType, exprInsts) <- pureInfer(exprTerm)
          caseType <- cases.foldRight(
            (None: Option[Type], List[Instantiation]()).pure[StateEither]
          ) { case ((pattern, term), acc) =>
            for {
              (a, accInsts) <- acc
              mark <- EitherT.liftF(addMark("m"))
              appExprType <- exprType match {
                case TypeEVar(_, _, _) => apply(exprType)(shift = false)
                case _                 => exprType.pure[StateEither]
              }
              (patternType, variables, patternInsts) <- inferPattern(
                pattern,
                appExprType
              )(checking)
              (caseExprType, caseExprInsts) <- pureInfer(term)
              _ <- a match {
                case None    => true.pure[StateEither]
                case Some(t) => subtype(caseExprType, t)
              }
              _ <- patternType
                .map(subtype(appExprType, _))
                .getOrElse(EitherT.pure[ContextState, Error](()))
              cT <- caseExprType match {
                case TypeEVar(_, _, _) => apply(caseExprType)(shift = false)
                case _                 =>
                  typeShift(-variables.length, caseExprType)
                    .pure[StateEither]
              }
              _ <- EitherT.liftF(peel(mark))
            } yield (Some(cT), accInsts ::: patternInsts ::: caseExprInsts)
          }
        } yield caseType match {
          case (t, insts) => (t.getOrElse(TypeUnit(info)), insts)
        }
      case TermTag(info, tag, t1, ty1) =>
        for {
          tyS <- EitherT.liftF(simplifyType(ty1))
          tySVar <- tyS match {
            case TypeRec(_, _, _, v @ TypeVariant(_, _)) => v.pure[StateEither]
            case _                                       =>
              TypeError.format(TagNotVariantTypeError(info, ty1))
          }
          tyTiExpected <- tySVar.v
            .find(_._1 == tag)
            .map { case (_, ty) =>
              typeSubstituteTop(findRootType(ty1), ty).pure[StateEither]
            }
            .getOrElse(
              TypeError.format(
                TagVariantFieldNotFoundTypeError(info, ty1, tag)
              )
            )
          (tyTi, insts) <- pureInfer(t1)
          _ <- isTypeEqualWithTypeError(
            tyTi,
            tyTiExpected,
            TagFieldMismatchTypeError(info, tyTi, tyTiExpected)
          )
        } yield (typeSubstituteTop(ty1, tySVar), insts)
      case TermTrue(info)               => (TypeBool(info), Nil).pure
      case TermFalse(info)              => (TypeBool(info), Nil).pure
      case TermInt(info, _)             => (TypeInt(info), Nil).pure
      case TermFloat(info, _)           => (TypeFloat(info), Nil).pure
      case TermString(info, _)          => (TypeString(info), Nil).pure
      case TermUnit(info)               => (TypeUnit(info), Nil).pure
      case TermBuiltin(ty)              => (ty, Nil).pure
      case TermClassMethod(info, ty, _) => (ty, Nil).pure
    }

  /** Infers the type of an application of a function of type `fun` to `exp`.
    *
    * See Figure 11.
    * @return
    *   the inferred type with solutions for existential types on the fun type
    *   and the output context
    */
  def inferApp(
      funType: Type,
      exp: Term
  ): StateEither[(Type, (List[Instantiation], List[TypeESolutionBind]))] =
    def iter(
        funType: Type,
        exp: Term,
        acc: (List[Instantiation], List[TypeESolutionBind])
    ): StateEither[(Type, (List[Instantiation], List[TypeESolutionBind]))] =
      funType match {
        case TypeAll(info, _, _, cls, tpe) =>
          for {
            eV <- EitherT.liftF(addEVar("a", exp.info, TypeEFreeBind, cls))
            reduced = typeSubstituteTop(eV, tpe)
            (ty, (insts, solutions)) <- iter(reduced, exp, acc)
          } yield (ty, (acc._1 ::: insts, acc._2 ::: solutions))
        case eA: TypeEVar =>
          for {
            (a1, a2, s1) <- insertEArrow(eA)
            (insts, solutions) <- check(exp, a1)
          } yield (a2, (acc._1 ::: insts, acc._2 ::: (s1 :: solutions)))
        case TypeArrow(_, argT, restT) =>
          check(exp, argT).map { case (insts, solutions) =>
            (restT, (acc._1 ::: insts, acc._2 ::: solutions))
          }
        case _ =>
          TypeError.format(VariableNotFunctionTypeError(exp.info, funType))
      }
    iter(funType, exp, (List(), List()))

  def inferMethod(
      ty: Type,
      simplifiedType: Type,
      typeBounds: List[TypeClass],
      method: String,
      info: Info
  ): StateEither[Type] = {
    (simplifiedType, findRootTypeVar(ty), typeBounds) match {
      case (_: TypeRec | _: TypeAbs, Some(rootTypeVar), _) =>
        getTypeMethod(info, ty, rootTypeVar, method)
      case (_: TypeVar | _: TypeApp, _, cls) =>
        inferTypeClassMethod(info, method, cls, simplifiedType)
      case (TypeAll(_, _, _, _, tpe), _, _) =>
        TypeError.format(
          MissingTypeAnnotation(info, typeShift(-1, simplifiedType))
        )
      case _ =>
        TypeError.format(NoMethodsOnTypeError(info, simplifiedType))
    }
  }

  def inferTypeClassMethod(
      info: Info,
      method: String,
      cls: List[TypeClass],
      simplifiedType: Type
  ): StateEither[Type] =
    for {
      methodTypes <- cls.traverse(getTypeClassMethodType(_, method))
      ty <- methodTypes match {
        case h :: Nil => h.pure[StateEither]
        case Nil      =>
          TypeError.format(NoMethodsOnTypeError(info, simplifiedType))
        case _ =>
          TypeError.format(
            MultipleTypeClassMethodsFound(info, cls, method)
          )
      }
    } yield ty

  def typeErrorEitherCond[T](
      cond: Boolean,
      value: T,
      error: TypeError
  ): StateEither[T] =
    cond match {
      case true  => value.pure[StateEither]
      case false => TypeError.format(error)
    }

  def inferPattern(
      p: Pattern,
      matchExprType: Type
  )(implicit
      checking: Boolean = true
  ): StateEither[(Option[Type], List[String], List[Instantiation])] =
    p match {
      case t: Term => infer(t).map { (ty, insts) => (Some(ty), List(), insts) }
      case PatternDefault(_)        => EitherT.pure((None, List(), List()))
      case n @ PatternNode(_, _, _) =>
        inferNodePattern(n, matchExprType)(checking)
    }

  def inferNodePattern(
      p: PatternNode,
      matchExprType: Type
  )(implicit
      checking: Boolean
  ): StateEither[(Option[Type], List[String], List[Instantiation])] = for {
    unfoldedExprType <- EitherT.liftF(unfoldType(matchExprType))
    v <- unfoldedExprType match {
      case TypeVariant(_, fields) =>
        for {
          ty <- fields
            .find(_._1 == p.tag)
            .map { case (_, v) => v.pure[StateEither] }
            .getOrElse(
              TypeError.format(
                MatchVariantPatternMismatchTypeError(
                  p.info,
                  matchExprType,
                  p.tag
                )
              )
            )
          variables <- ty match {
            case tyR @ TypeRecord(_, _) =>
              inferPattern(p, tyR)(checking).map(_._2)
            case _ => List().pure[StateEither]
          }
        } yield (Some(matchExprType), variables, List())
      case TypeRecord(_, fields) =>
        for {
          // NOTE: In case we're not in process of type checking, there's no
          // need to do additional type fetching/checking on the pattern tag
          // type. Useful during code generation as we can escape further
          // monomorphization for specialized type patterns.
          _ <- checking match {
            case true =>
              val idx = EitherT.liftF(
                State.inspect((ctx: Context) => Context.nameToIndex(ctx, p.tag))
              )
              idx.flatMap(i =>
                i.map(getType(p.info, _))
                  .getOrElse(
                    TypeError.format(
                      MatchRecordPatternMismatchTypeError(
                        p.info,
                        matchExprType,
                        p.tag
                      )
                    )
                  )
              )
            case false => ().pure[StateEither]
          }
          variables <- bindFieldsToVars(
            p.info,
            fields,
            p.vars,
            matchExprType
          )
        } yield (Some(matchExprType), variables, List())
      case TypeEVar(info, _, _) => inferNodePatternWithEVar(info, p)
      case _                    =>
        TypeError.format(
          MatchExprNotDataTypeError(p.info, matchExprType, p.tag)
        )
    }
  } yield v

  def inferNodePatternWithEVar(
      info: Info,
      p: PatternNode
  )(implicit
      checking: Boolean = true
  ): StateEither[(Option[Type], List[String], List[Instantiation])] = for {
    optionType <- EitherT.liftF(getAlgebraicDataTypeVar(info, p.tag))
    v <- optionType match {
      case Some(ty) =>
        for {
          tyS <- EitherT.liftF(simplifyType(ty))
          eVars <- EitherT.liftF(extractEVarsFromTypeAbs(tyS))
          tyA = eVars.foldLeft(ty: Type)((acc, eA) => TypeApp(eA.info, acc, eA))
          (tP, vars, insts) <- inferPattern(p, tyA)(checking)
        } yield (tP, vars, insts)
      case _ => TypeError.format(MatchNodePatternNotFound(p.info, p))
    }
  } yield v

  def extractEVarsFromTypeAbs(ty: Type): ContextState[List[TypeEVar]] = {
    def iter(t: Type, acc: List[TypeEVar]) = t match {
      case TypeAbs(info, i, tyB) =>
        for {
          eA <- addEVar(i, info, TypeEFreeBind)
          a <- extractEVarsFromTypeAbs(tyB)
        } yield eA :: a
      case _ => acc.pure[ContextState]
    }
    iter(ty, List())
  }

  def bindFieldsToVars(
      info: Info,
      fields: List[(String, Type)],
      vars: List[String],
      exprType: Type
  ): StateEither[List[String]] =
    fields.length == vars.length match {
      case true =>
        val t = fields.zipWithIndex.zip(vars).traverse { case ((f, i), v) =>
          isRecursiveType(f._2).map(isRecType => {
            val variableName = isRecType match {
              case true  => s"$v'"
              case false => v
            }
            (variableName, f._2, i)
          })
        }
        EitherT.right(
          t.flatMap(v =>
            v.traverse { case (v, ty, idx) =>
              Context.addBinding(v, VarBind(typeShift(idx, ty)))
            }
          )
        )
      case false =>
        TypeError.format(
          MatchPatternWrongVarsTypeError(
            info,
            exprType,
            fields.length,
            vars.length
          )
        )
    }

  def unfoldType(tyS: Type): ContextState[Type] =
    simplifyType(tyS).map(_ match {
      case TypeRec(_, _, _, tyT) =>
        typeSubstituteTop(findRootType(tyS), tyT)
      case ty => ty
    })

  def simplifyType(ty: Type): ContextState[Type] =
    for {
      tyA <- apply(ty)(shift = false).getOrElse(ty)
      tyT <- tyA match {
        case TypeApp(info, tyT1, tyT2) =>
          simplifyType(tyT1).map(TypeApp(info, _, tyT2))
        // NOTE: Here we return the original type, not the one ewe tried to apply.
        // Since we wanna extract type application only for applied types.
        case _ => ty.pure[ContextState]
      }
      tyT1 <- computeType(tyT).semiflatMap(simplifyType(_)).getOrElse(tyT)
    } yield tyT1

  def computeType(ty: Type): StateOption[Type] = ty match {
    case TypeVar(_, idx, _)                        => getTypeAbb(idx)
    case TypeApp(info, TypeAbs(_, _, tyT12), tyT2) =>
      OptionT.some[ContextState](typeSubstituteTop(tyT2, tyT12))
    case TypeApp(info, TypeId(_, name), tyT2) =>
      // Handle TypeApp(TypeId, arg) by looking up the TypeId and applying the argument
      for {
        idx <- OptionT.liftF[ContextState, Option[Int]](State.inspect {
          (ctx: Context) =>
            Context.nameToIndex(ctx, name)
        })
        index <- OptionT.fromOption[ContextState](idx)
        typeAbb <- getTypeAbb(index)
        // The type abbreviation should be a TypeAbs, apply tyT2 to it
        result <- typeAbb match {
          case TypeAbs(_, _, tyT12) =>
            OptionT.some[ContextState](typeSubstituteTop(tyT2, tyT12))
          case _ =>
            OptionT.none[ContextState, Type] // Not a type abstraction
        }
      } yield result
    case _ => OptionT.none[ContextState, Type]
  }

  @tailrec
  def findRootType(ty: Type): Type = ty match {
    case v @ TypeVar(_, _, _) => v
    case TypeApp(_, ty1, ty2) => findRootType(ty1)
    case _                    => ty
  }

  @tailrec
  def findRootTypeVar(ty: Type): Option[TypeVar] = ty match {
    case v: TypeVar           => Some(v)
    case TypeApp(_, ty1, ty2) => findRootTypeVar(ty1)
    case _                    => None
  }

  def getTypeAbb(index: Int): StateOption[Type] =
    Context
      .getBinding(UnknownInfo, index)
      .toOption
      .flatMap(_ match {
        case TypeAbbBind(ty, _) => OptionT.some(ty)
        case _                  => OptionT.none
      })

  def getTypeBounds(ty: Type): ContextState[List[TypeClass]] = ty match {
    case TypeVar(info, index, _) =>
      Context
        .getBinding(UnknownInfo, index)
        .getOrElse(List())
        .map(_ match {
          case TypeVarBind(_, cls) => cls
          case _                   => List()
        })
    case _ => List().pure
  }

  def isTypeEqualWithTypeError(
      ty1: Type,
      ty2: Type,
      error: TypeError
  ): StateEither[Type] = for {
    isEqual <- EitherT.liftF(isTypeEqual(ty1, ty2))
    t <- typeErrorEitherCond(
      isEqual,
      ty1,
      error
    )
  } yield t

  def isSubtypeWithTypeError(
      ty1: Type,
      ty2: Type,
      error: TypeError
  ): StateEither[Type] = for {
    (isEqual, _) <- subtype(ty1, ty2)
    t <- typeErrorEitherCond(
      isEqual,
      ty1,
      error
    )
  } yield t

  def isRecursiveType(ty: Type): ContextState[Boolean] =
    simplifyType(ty)
      .map(_ match {
        case _: TypeRec => true
        case v          => false
      })

  def pureKindOf(t: Type): StateEither[Kind] = Context.runE(kindOf(t))

  def kindOf(t: Type): StateEither[Kind] = t match {
    case TypeArrow(_, ty1, ty2) =>
      for {
        _ <- checkKindStar(ty1)
        _ <- checkKindStar(ty1)
      } yield KindStar
    case TypeVar(info, idx, _) => getKind(info, idx)
    case TypeId(info, name)    =>
      // TypeId might be a monomorphized type or a type constructor
      // Try to look it up in the context
      for {
        idxOpt <- EitherT.liftF(
          State.inspect[Context, Option[Int]](ctx =>
            Context.nameToIndex(ctx, name)
          )
        )
        kind <- idxOpt match {
          case Some(idx) => getKind(info, idx)
          case None      =>
            // Not found in context - might be a monomorphized name like "List#i32"
            // These are fully applied types, so they have kind *
            EitherT.rightT[ContextState, String](KindStar)
        }
      } yield kind
    case TypeRecord(_, fields) =>
      fields
        .traverse(f => checkKindStar(f._2))
        .map(_ => KindStar)
    case TypeAll(_, tyX, k1, cls, tyT2) =>
      for {
        _ <- EitherT.liftF(Context.addBinding(tyX, TypeVarBind(k1, cls)))
        _ <- checkKindStar(tyT2)
      } yield KindStar
    case TypeAbs(_, tyX, tyT2) =>
      for {
        _ <- EitherT.liftF(addBinding(tyX, TypeVarBind(KindStar)))
        k <- pureKindOf(tyT2)
      } yield KindArrow(KindStar, k)
    case TypeApp(info, tyT1, tyT2) =>
      for {
        k1 <- pureKindOf(tyT1)
        k2 <- pureKindOf(tyT2)
        k <- k1 match {
          case KindArrow(k11, k12) =>
            typeErrorEitherCond(
              k11 == k2,
              k12,
              KindParameterMismatchTypeError(
                info,
                tyT2,
                k2,
                k11
              )
            )
          case _ => TypeError.format(NoTypeArgumentsTypeError(info, tyT1))
        }
      } yield k
    case TypeRec(_, tyX, k1, tyT2) =>
      for {
        _ <- EitherT.liftF(addBinding(tyX, TypeVarBind(k1)))
        k <- pureKindOf(tyT2)
      } yield k
    case _ => EitherT.rightT(KindStar)
  }

  def checkKindStar(ty: Type): StateEither[Kind] =
    pureKindOf(ty).flatMap { k =>
      typeErrorEitherCond(
        k == KindStar,
        KindStar,
        InvalidTypeArgumentTypeError(ty.info, ty)
      )
    }

  def checkTypeInstance(name: String, b: Binding): StateEither[Unit] =
    (name, b) match {
      case (
            Desugar.TypeInstancePattern(cls, ty),
            TypeClassInstanceBind(_, _, m2)
          ) =>
        for {
          m1 <- EitherT.liftF(getTypeClassMethods(cls))
          v <- difference(m1, m2) match {
            case Nil  => ().pure[StateEither]
            case diff =>
              TypeError.format(
                MissingTypeInstanceMethods(UnknownInfo, ty, cls, diff)
              )
          }
        } yield v
      case _ => ().pure
    }

  def checkTypeInstanceMethodBinding(
      name: String,
      b: Binding
  ): StateEither[Unit] = (name, b) match {
    case (
          Desugar.TypeInstanceMethodPattern(method, typeName, className),
          TermAbbBind(_, Some(ty))
        ) =>
      for {
        typeClassMethodType <- getTypeClassMethodType(
          TypeClass(ty.info, className),
          method
        )
        (isSubType, _) <- subtype(typeClassMethodType, ty)
        v <- isSubType match {
          case true  => ().pure[StateEither]
          case false =>
            TypeError.format(
              InvalidTypeInstanceMethod(
                ty.info,
                ty,
                typeClassMethodType,
                method,
                typeName,
                className
              )
            )
        }
      } yield v
    case _ => ().pure
  }

  def getTypeClassKind(cls: TypeClass): StateEither[Kind] =
    for {
      idx <- EitherT.liftF(
        State.inspect((ctx: Context) =>
          Context
            .nameToIndex(ctx, cls.name)
        )
      )
      k <- idx match {
        case Some(i) => getKind(cls.info, i)
        case None    => TypeError.format(TypeClassNotFound(cls.info, cls.name))
      }
    } yield k

  def getKind(info: Info, idx: Int): StateEither[Kind] =
    Context
      .getBinding(info, idx)
      .flatMap(_ match {
        case TypeVarBind(k, _)       => k.pure
        case TypeAbbBind(_, Some(k)) => k.pure
        case TypeAbbBind(_, None)    =>
          TypeError.format(NoKindForTypeError(info, idx))
        case TypeClassBind(k) => k.pure
        case _ => TypeError.format(BindingNotFoundTypeError(info))
      })

  /** Checks (solves exist vars) that `exp` has type `t` with input context
    * `ctx`.
    */
  def check(
      exp: Term,
      t: Type
  ): StateEither[(List[Instantiation], List[TypeESolutionBind])] =
    (exp, t) match {
      // 1I :: ((), 1)
      case (_, _: TypeAny)                   => (Nil, Nil).pure
      case (TermUnit(_), TypeUnit(_))        => (Nil, Nil).pure
      case (TermFloat(_, _), TypeFloat(_))   => (Nil, Nil).pure
      case (TermString(_, _), TypeString(_)) => (Nil, Nil).pure
      case (TermInt(_, _), TypeInt(_))       => (Nil, Nil).pure
      case (TermTrue(_), TypeBool(_))        => (Nil, Nil).pure
      case (TermFalse(_), TypeBool(_))       => (Nil, Nil).pure
      // ->I :: (λx.e, A→B)
      case (TermClosure(_, arg, None, exp), TypeArrow(_, argT, expT)) =>
        for {
          b <- EitherT.liftF(addBinding(arg, VarBind(argT)))
          (insts, sol) <- check(exp, expT) // Γ,α ⊢ e ⇐ A ⊣ ∆,α,Θ
          // NOTE: The `argT` parameter is reduced and included as
          // type solution, as its solution might be lost in the
          // inferring of a nested expression.
          redArgT <- apply(argT)
          _ <- EitherT.liftF(peel(b))
        } yield (insts, sol :+ TypeESolutionBind(redArgT))
      // ∀I :: (e, ∀α.A)
      case (exp, TypeAll(_, uA, k, cls, tpe)) =>
        for {
          b <- EitherT.liftF(addBinding(uA, TypeVarBind(k, cls)))
          insts <- check(exp, tpe)
          _ <- EitherT.liftF(peel(b))
        } yield insts
      // Sub :: (e, B)
      case (exp, tpe) =>
        for {
          (expType, insts) <- infer(exp) // Γ ⊢ e ⇒ A ⊣ Θ
          redExpType <- apply(expType)
          reducedType <- apply(tpe)
          (r, solutions) <- subtype(redExpType, reducedType)
          _ <- r match {
            case true  => ().pure[StateEither]
            case false =>
              TypeError.format(
                InvalidSubtypeTypeError(exp.info, redExpType, reducedType)
              )
          }
        } yield (insts, solutions)
    }

  /** Applies `ctx` to `inst` (substituting existential vars for their solutions
    * for instantation types).
    */
  def applyInst(
      inst: Instantiation
  )(implicit shift: Boolean = true): StateEither[Instantiation] =
    inst.tys
      .traverse(applyC(_)(shift = shift))
      .map(v => v.unzip)
      .map { case (tys, cls) =>
        Instantiation(inst.i, inst.term, tys, inst.cls ::: cls.flatten, inst.r)
      }

  /** Applies `ctx` to `tpe` (substituting existential vars for their
    * solutions).
    */
  def apply(t: Type)(implicit shift: Boolean = true): StateEither[Type] =
    applyC(t).map(_._1)

  def applyC(
      t: Type
  )(implicit shift: Boolean = true): StateEither[(Type, List[TypeClass])] =
    t match {
      case ev: TypeEVar =>
        EitherT
          .liftF(Context.solution(ev, shift))
          .flatMap(
            _.map { case (ty1, cls1) =>
              applyC(ty1).map { case (ty2, cls2) => (ty2, cls1 ::: cls2) }
            }.getOrElse((ev: Type, ev.cls).pure)
          )
      case TypeArrow(info, a, b) =>
        for {
          (t1, cls1) <- applyC(a)
          (t2, cls2) <- applyC(b)
        } yield (TypeArrow(info, t1, t2), cls1 ::: cls2)
      case TypeApp(info, a, b) =>
        for {
          (t1, cls1) <- applyC(a)
          (t2, cls2) <- applyC(b)
        } yield (TypeApp(info, t1, t2), cls1 ::: cls2)
      case TypeAll(info, i, k, cls, tpe) =>
        applyC(tpe).map { case (t, c) =>
          (TypeAll(info, i, k, cls, t), c)
        }
      case _ => (t, List[TypeClass]()).pure
    }

  /** Derives a subtyping relationship `tpeA <: tpeB` with input context `ctx`.
    * See Figure 9.
    * @return
    *   true value if `tpeA` is subtype of `typB`
    */
  def subtype(
      tpeA: Type,
      tpeB: Type
  ): StateEither[(Boolean, List[TypeESolutionBind])] =
    (tpeA, tpeB) match {
      // <:Exvar :: Γ[â] ⊢ â <: â ⊣ Γ[â]
      case (eA @ TypeEVar(info, n1, _), TypeEVar(_, n2, _)) if (n1 == n2) =>
        EitherT
          .liftF(containsFreeEVar(eA))
          .flatMap(_ match {
            case true  => (true, List()).pure
            case false =>
              TypeError.format(UnboundExistentialVariableTypeError(info, eA))
          })
      // <:→ :: Γ ⊢ A1→A2 <: B1→B2 ⊣ ∆
      case (TypeArrow(_, a1, a2), TypeArrow(_, b1, b2)) =>
        for {
          (s1, sol1) <- subtype(b1, a1)
          redA2 <- apply(a2)(shift = false)
          redB2 <- apply(b2)(shift = false)
          (s2, sol2) <- subtype(redA2, redB2)
        } yield (s1 && s2, sol1 ::: sol2)
      case (TypeApp(_, a1, a2), TypeApp(_, b1, b2)) =>
        for {
          // NOTE: When dealing with closures we could recieve type vars that
          // are not shifted properly after adding existential var bindings,
          // thus we need to resolve this by shifting the context.
          b1S <- EitherT.liftF(typeShiftOnContextDiff(b1))
          (s1, sol1) <- subtype(b1S, a1)
          redA2 <- apply(a2)(shift = false)
          redB2 <- apply(b2)(shift = false)
          (s2, sol2) <- subtype(redA2, redB2)
        } yield (s1 && s2, sol1 ::: sol2)
      // <:∀L :: Γ ⊢ ∀α.A <: B ⊣ ∆
      case (TypeAll(info, _, _, cls, a), b) =>
        for {
          eAMark <- EitherT.liftF(Context.addMark("a"))
          eA <- EitherT.liftF(Context.addEVar("a", info, TypeEFreeBind, cls))
          r <- subtype(typeSubstituteTop(eA, a), b)
          _ <- EitherT.liftF(peel(eAMark))
        } yield r
      // <:∀R :: Γ ⊢ A <: ∀α.B ⊣ ∆
      case (a, TypeAll(_, uB, k, cls, b)) =>
        for {
          n <- EitherT.liftF(Context.addBinding(uB, TypeVarBind(k, cls)))
          r <- subtype(a, b)
          _ <- EitherT.liftF(peel(n))
        } yield r
      // <:InstantiateL :: Γ[â] ⊢ â <: A ⊣ ∆
      case (eA: TypeEVar, a) =>
        for {
          isFree <- EitherT.liftF(Context.containsFreeEVar(eA))
          hasFree = a.containsEVar(eA)
          r <- isFree && !hasFree match {
            case false => (false, List()).pure[StateEither]
            case true  => instantiateL(eA, a).map((true, _))
          }
        } yield r
      // <:InstantiateR :: Γ[â] ⊢ A <: â ⊣ ∆
      case (a, eA: TypeEVar) =>
        for {
          isFree <- EitherT.liftF(Context.containsFreeEVar(eA))
          hasFree = a.containsEVar(eA)
          r <- isFree && !hasFree match {
            case false => (false, List()).pure[StateEither]
            case true  => instantiateR(a, eA).map((true, _))
          }
        } yield r
      case _ => EitherT.liftF(isTypeEqual(tpeA, tpeB)).map((_, List()))
    }

  /** Instantiates `eA` such that `eA <: a` in `ctx`. See Figure 10.
    * @return
    *   the output context.
    */
  def instantiateL(
      eA: TypeEVar,
      a: Type
  ): StateEither[List[TypeESolutionBind]] = {
    def instantiate(
        isSimple: Boolean,
        containsEVarWithPeel: Boolean,
        containsEVar: Boolean
    ): StateEither[List[TypeESolutionBind]] =
      (a, isSimple, containsEVarWithPeel, containsEVar) match {
        case (a, true, _, _) /* Γ ⊢ τ */ =>
          solve(eA, a).map(List(_)) // Γ,â=τ,Γ′
        case (eC: TypeEVar, _, true, _)       => solve(eC, eA).map(List(_))
        case (TypeApp(_, a1, a2), _, _, true) =>
          for {
            (eA1, eA2, sol1) <- insertEApp(eA)
            sR <- instantiateR(a1, eA1)
            redA2 <- apply(a2)(shift = false)
            sL <- instantiateL(eA2, redA2)
          } yield sol1 :: sR ::: sL
        // InstLArr :: Γ[â] ⊢ â :=< A1 → A2 ⊣ ∆
        case (TypeArrow(info, a1, a2), _, _, true) =>
          for {
            (eA1, eA2, sol1) <- insertEArrow(eA)
            sR <- instantiateR(a1, eA1)
            redA2 <- apply(a2)(shift = false)
            sL <- instantiateL(eA2, redA2)
          } yield sol1 :: sR ::: sL
        // InstLAllR :: Γ[â] ⊢ â :=< ∀β.B ⊣ ∆
        case (TypeAll(_, uB, k, cls, b), _, _, true) =>
          for {
            v <- EitherT.liftF(Context.addBinding(uB, TypeVarBind(k, cls)))
            // Γ[â],β ⊢ â :=< B ⊣ ∆,β,∆′
            r <- instantiateL(eA, b)
            _ <- EitherT.liftF(peel(v))
          } yield r
        case _ => TypeError.format(FailedToInstantiateTypeError(eA.info, eA, a))
      }
    for {
      b1 <- isMonoAndWellFormed(a, eA)
      b2 <- EitherT.liftF(containsEVarWithPeel(eA, a))
      b3 <- EitherT.liftF(containsFreeEVar(eA))
      v <- instantiate(b1, true, b3)
    } yield v
  }

  /** Instantiates `eA` such that `a <: eA` in `ctx`. See Figure 10.
    * @return
    *   the output context.
    */
  def instantiateR(
      a: Type,
      eA: TypeEVar
  ): StateEither[List[TypeESolutionBind]] = {
    def instantiate(
        isSimple: Boolean,
        containsEVarWithPeel: Boolean,
        containsEVar: Boolean
    ): StateEither[List[TypeESolutionBind]] =
      (a, isSimple, containsEVarWithPeel, containsEVar) match {
        case (a, true, _, _) /* Γ ⊢ τ */ =>
          solve(eA, a).map(List(_)) // Γ,â=τ,Γ′
        case (eC: TypeEVar, _, true, _)       => solve(eC, eA).map(List(_))
        case (TypeApp(_, a1, a2), _, _, true) =>
          for {
            (eA1, eA2, sol1) <- insertEApp(eA)
            sL <- instantiateL(eA1, a1)
            redA2 <- apply(a2)(shift = false)
            sR <- instantiateR(redA2, eA2)
          } yield sol1 :: sL ::: sR
        // InstRArr :: Γ[â] ⊢ A1 → A2 :=< â ⊣ ∆
        case (TypeArrow(info, a1, a2), _, _, true) =>
          for {
            (eA1, eA2, sol1) <- insertEArrow(eA)
            sL <- instantiateL(eA1, a1)
            redA2 <- apply(a2)(shift = false)
            sR <- instantiateR(redA2, eA2)
          } yield sol1 :: sL ::: sR
        // InstRAllL :: Γ[â],▶ĉ,ĉ ⊢ [ĉ/β]B :=< â ⊣ ∆,▶ĉ,∆′
        case (TypeAll(info, _, _, cls, b), _, _, true) =>
          for {
            eCMark <- EitherT.liftF(Context.addMark("c"))
            eC <- EitherT.liftF(Context.addEVar("c", info, TypeEFreeBind, cls))
            // Γic ⊢ [ĉ/β]B :=< â ⊣ ∆,▶ĉ,∆′
            r <- instantiateR(typeSubstituteTop(eC, b), eA)
            _ <- EitherT.liftF(peel(eCMark)) // ∆
          } yield r
        case _ => TypeError.format(FailedToInstantiateTypeError(eA.info, a, eA))
      }
    for {
      b1 <- isMonoAndWellFormed(a, eA)
      b2 <- EitherT.liftF(containsEVarWithPeel(eA, a))
      b3 <- EitherT.liftF(containsFreeEVar(eA))
      v <- instantiate(b1, b2, b3)
    } yield v
  }

  def isMonoAndWellFormed(a: Type, eA: TypeEVar): StateEither[Boolean] =
    a.isMono match {
      case true  => EitherT.liftF(isWellFormedWithPeel(a, eA))
      case false => false.pure
    }

  def solve(eA: TypeEVar, ty: Type): StateEither[TypeESolutionBind] = for {
    (idx, cls) <- getFreeEVarIndex(eA)
    z <- cls match {
      case Nil => EitherT.liftF(replaceEVar(idx, eA, TypeESolutionBind(ty)))
      // NOTE: In case there are type instances for to the existential
      // variable, we need to:
      // * augment the type instances in case of another existential variable
      // * solve the variable if type instances are equal to the type and
      // if the type bounds are met
      // * throw an error if there are no type instances found or type instances
      // are not bound to the type
      case l =>
        for {
          instances <- EitherT.liftF(
            l.traverse(c =>
              getClassIntances(c).map(_.zip(LazyList.continually(c)))
            ).map(_.flatten)
          )
          rootType = findRootType(ty)
          implCls <- instances
            .traverse { case (tyi, c) =>
              EitherT.liftF(isTypeEqual(tyi, rootType)).map((_, c))
            }
            .map(_.collect({ case (true, c) => c }))
          typeBounds <- EitherT.liftF(getTypeBounds(rootType))
          v <- (
            !implCls.isEmpty,
            ty,
            isTypeClassBound(l, typeBounds)
          ) match {
            case (false, TypeEVar(info, name, cls2), _) =>
              EitherT.liftF(
                replaceEVar(
                  idx,
                  eA,
                  TypeESolutionBind(TypeEVar(info, name, union(cls, cls2)))
                )
              )
            case (false, _, false) =>
              TypeError.format(
                TypeClassInstanceNotFound(
                  ty.info,
                  cls,
                  instances.unzip._1,
                  rootType
                )
              )
            case _ =>
              EitherT.liftF(
                replaceEVar(idx, eA, TypeESolutionBind(ty, union(implCls, cls)))
              )
          }
        } yield v
    }
  } yield z

  def isTypeClassBound(
      requiredTypeClasses: List[TypeClass],
      boundTypeClasess: List[TypeClass]
  ): Boolean = requiredTypeClasses.forall(c =>
    boundTypeClasess.exists { case TypeClass(_, n) => c.name == n }
  )

  def isTypeEqual(ty1: Type, ty2: Type): ContextState[Boolean] = Context.run {
    (ty1, ty2) match {
      case (TypeString(_), TypeString(_))   => true.pure
      case (TypeId(_, id1), TypeId(_, id2)) => (id1 == id2).pure
      case (TypeUnit(_), TypeUnit(_))       => true.pure
      case (TypeFloat(_), TypeFloat(_))     => true.pure
      case (TypeInt(_), TypeInt(_))         => true.pure
      case (TypeBool(_), TypeBool(_))       => true.pure
      case (TypeArrow(_, tyS1, tyS2), TypeArrow(_, tyT1, tyT2)) =>
        for {
          b1 <- isTypeEqual(tyS1, tyT1)
          b2 <- isTypeEqual(tyS2, tyT2)
        } yield b1 && b2
      case (TypeRec(_, x1, k1, tyS1), TypeRec(_, _, k2, tyT1)) if k1 == k2 =>
        Context.addName(x1).flatMap(_ => isTypeEqual(tyS1, tyT1))
      case (TypeVar(_, idx1, len1), TypeVar(_, idx2, len2)) =>
        (idx1 == idx2).pure
      case (TypeRecord(_, f1), TypeRecord(_, f2)) if f1.length == f2.length =>
        f1.traverse { case (l1, tyT1) =>
          f2.find(_._1 == l1)
            .map(f => isTypeEqual(tyT1, f._2))
            .getOrElse(false.pure[ContextState])
        }.map(_.forall(identity))
      case (TypeVariant(_, f1), TypeVariant(_, f2)) if f1.length == f2.length =>
        f1.traverse { case (l1, tyT1) =>
          f2.find(_._1 == l1)
            .map(f => isTypeEqual(tyT1, f._2).map(_ && l1 == f._1))
            .getOrElse(false.pure[ContextState])
        }.map(_.forall(identity))
      case (TypeAll(_, tyX1, k1, cls1, tyS1), TypeAll(_, _, k2, cls2, tyT1))
          if k1 == k2 && cls1 == cls2 =>
        Context.addName(tyX1).flatMap(_ => isTypeEqual(tyS1, tyT1))
      case (TypeAbs(_, tyX1, tyS1), TypeAbs(_, _, tyT1)) =>
        Context.addName(tyX1).flatMap(_ => isTypeEqual(tyS1, tyT1))
      case (TypeApp(_, tyS1, tyS2), TypeApp(_, tyT1, tyT2)) =>
        for {
          b1 <- isTypeEqual(tyS1, tyT1)
          b2 <- isTypeEqual(tyS2, tyT2)
        } yield b1 && b2
      case (TypeClass(_, c1), TypeClass(_, c2)) => (c1 == c2).pure
      case _                                    => false.pure
    }
  }

  /** Resolve type constructor TypeVars to TypeIds while preserving type
    * parameter TypeVars.
    *
    * This is critical for closure types: type constructors (like "List") are
    * captured as TypeVars with De Bruijn indices during type checking. These
    * indices become invalid when the context changes during monomorphization.
    * By resolving them to TypeIds (names) during type checking when the context
    * is still valid, we preserve the type constructor information across
    * compilation phases.
    *
    * For TypeApp(TypeVar(9), TypeVar(6)) representing List[B]:
    *   - TypeVar(9) points to "List" (type constructor) → resolve to
    *     TypeId("List")
    *   - TypeVar(6) points to "B" (type parameter) → keep as TypeVar for
    *     specialization
    *
    * @param ty
    *   The type to process
    * @param ctx
    *   The current context (used to resolve TypeVar indices)
    * @return
    *   Type with constructor TypeVars resolved to TypeIds
    */
  private def resolveTypeConstructors(ty: Type, ctx: Context): Type = ty match {
    case TypeApp(info, ctor, param) =>
      // Resolve constructor if it's a TypeVar
      val resolvedCtor = ctor match {
        case TypeVar(_, idx, _) =>
          // Look up the binding at this index
          Context.indexToName(ctx, idx) match {
            case Some(name) =>
              // Check if this is a type constructor (starts with uppercase or is a known type)
              if (name.headOption.exists(_.isUpper) && !name.contains("#")) {
                TypeId(info, name)
              } else {
                ctor // Keep as TypeVar (it's a type parameter, not constructor)
              }
            case None =>
              ctor
          }
        case _ =>
          // Already resolved or not a TypeVar
          resolveTypeConstructors(ctor, ctx)
      }
      // Keep param as-is (it might be a type parameter that needs substitution later)
      TypeApp(info, resolvedCtor, param)

    case TypeArrow(info, t1, t2) =>
      TypeArrow(
        info,
        resolveTypeConstructors(t1, ctx),
        resolveTypeConstructors(t2, ctx)
      )

    case TypeAll(info, name, kind, cls, t) =>
      TypeAll(info, name, kind, cls, resolveTypeConstructors(t, ctx))

    case _ => ty // Other types remain unchanged
  }
}
