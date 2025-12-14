package core

import cats.data.EitherT
import cats.data.State
import cats.data.StateT
import cats.implicits.*
import core.Bindings.*
import core.Context.*
import core.Terms.*
import core.Types.*
import parser.Expressions.*
import parser.FuseParser.*
import parser.Identifiers.*
import parser.Info.ShowInfo.*
import parser.Info.*
import parser.Types.*
import core.BuiltIn.*

// TODO: Add constants for prefixes for specific names in the context.

object Desugar {
  val RecursiveFunctionParamPrefix = "^"
  val MethodNamePrefix = "!"
  val RecordConstrPrefix = "%"
  val SelfTypeName = "Self"
  val SelfIdentifier = "self"
  val FlatMapMethod = "flat_map"
  val MapMethod = "map"

  // Patterns
  val AlphaNum = "[a-zA-Z0-9]+";
  val TypeInstanceMethodPattern =
    s"$MethodNamePrefix($AlphaNum)#($AlphaNum)#($AlphaNum)".r
  val TypeInstancePattern =
    s"#($AlphaNum)#($AlphaNum)".r
  val MethodPattern = s"$MethodNamePrefix($AlphaNum)#($AlphaNum)".r

  def run(
      decls: List[FDecl],
      initContext: Context
  ): Either[Error, List[Bind]] =
    process(decls).value.runA(initContext).value

  def process(decls: List[FDecl]): StateEither[List[Bind]] =
    decls.traverse(bind(_)).map(_.flatten)

  // # Bind # region_start

  def bind(d: FDecl): StateEither[List[Bind]] = d match {
    case FVariantTypeDecl(info, FIdentifier(i), typ, values) =>
      val variant = toTypeVariant(info, values)
      val rec = withRecType(info, i, typ, variant)
      for {
        variantType <- Context.runE(withTypeAbs(typ, rec))
        typeBind <- bindTypeAbb(i, variantType)
        constructorBinds <- values.toList.traverse(v =>
          for {
            constructor <- Context.runE(
              buildVariantConstructor(info, i, typ, v)
            )
            constructorBind <- bindTermAbb(v.v.value, constructor)
          } yield constructorBind
        )
      } yield typeBind :: constructorBinds
    case FRecordTypeDecl(info, FIdentifier(i), typ, fields) =>
      val record = toTypeRecord(info, fields.map(_.p))
      val rec = withRecType(info, i, typ, record)
      for {
        recordType <- Context.runE(withTypeAbs(typ, rec))
        typeBind <- bindTypeAbb(i, recordType)
        constructor <- Context.runE(
          buildRecordConstructor(info, i, typ, Left(fields.map(_.p)))
        )
        constructorBind <- bindTermAbb(toRecordConstructorID(i), constructor)
      } yield typeBind :: List(constructorBind)
    case FTupleTypeDecl(info, FIdentifier(i), typ, types) =>
      val tuple = toTupleTypeRecord(info, types)
      val rec = withRecType(info, i, typ, tuple)
      for {
        recordType <- Context.runE(withTypeAbs(typ, rec))
        typeBind <- bindTypeAbb(i, recordType)
        constructor <- Context.runE(
          buildRecordConstructor(info, i, typ, Right(types))
        )
        constructorBind <- bindTermAbb(toRecordConstructorID(i), constructor)
      } yield typeBind :: List(constructorBind)
    case FTypeAlias(_, FIdentifier(i), typ, t) =>
      val alias = toType(t)
      for {
        alias <- Context.runE(withTypeAbs(typ, alias))
        typeBind <- bindTypeAbb(i, alias)
      } yield List(typeBind)
    case FFuncDecl(sig @ FFuncSig(_, FIdentifier(i), tp, _, _), exprs) => {
      for {
        func <- Context.runE(buildFunc(tp, sig, exprs))
        funcBind <- bindTermAbb(i, func)
      } yield List(funcBind)
    }
    case FTypeFuncDecls(info, typeIdentifier, typeParams, functions) =>
      val modifySignature = (sig: FMethodSig) =>
        val (typ, p) =
          toParamsWithSelf(sig.p, typeIdentifier, typeParams, sig.tp)
        FFuncSig(
          sig.info,
          FIdentifier(toMethodID(sig.i.value, typeIdentifier.value)),
          typ,
          p,
          sig.r
        )
      functions.toList
        .traverse(f => bind(FFuncDecl(modifySignature(f.sig), f.exprs)))
        .map(_.flatten)
    case FTraitDecl(info, traitIdentifier, typeParams, functions) =>
      // NOTE: Add a type paramater that has a type bound for the trait.
      val selfTypeParam = FTypeParam(
        info,
        FIdentifier(SelfTypeName),
        Some(Seq(FSimpleType(info, traitIdentifier))) // type class constraint
      )
      val modifySignature = (sig: FMethodSig) =>
        val (typ, p) =
          toParamsWithSelf(sig.p, FIdentifier(SelfTypeName), typeParams, sig.tp)
        FFuncSig(
          sig.info,
          FIdentifier(toMethodID(sig.i.value, traitIdentifier.value)),
          Some(selfTypeParam +: typ.toSeq.flatten),
          p,
          sig.r
        )
      val typeClass = TypeClass(info, traitIdentifier.value)
      for {
        bt <- bindTypeClass(traitIdentifier.value, typeParams)
        bf <- functions.toList
          .traverse {
            case Left(f)  => bind(FFuncDecl(modifySignature(f.sig), f.exprs))
            case Right(s) =>
              val ms = modifySignature(s)
              for {
                m <- Context.runE(toClassMethod(ms, typeParams, typeClass))
                ta <- bindTermAbb(ms.i.value, m)
              } yield List(ta)
          }
          .map(_.flatten)
      } yield bt +: bf
    case FTraitInstance(
          info,
          traitIdentifier,
          _,
          typeIdentifier,
          typeParams,
          methods
        ) =>
      val modifySignature = (sig: FMethodSig) =>
        val (typ, p) =
          toParamsWithSelf(sig.p, typeIdentifier, typeParams, sig.tp)
        FFuncSig(
          sig.info,
          FIdentifier(
            toTypeInstanceMethodID(
              sig.i.value,
              typeIdentifier.value,
              traitIdentifier.value
            )
          ),
          typ,
          p,
          sig.r
        )
      for {
        ty <- toType(FSimpleType(info, typeIdentifier))
        typeClassInstanceBind <- bindTypeClassInstance(
          traitIdentifier.value,
          typeIdentifier.value,
          ty,
          methods.map(_.sig.i.value).toList
        )
        methodBinds <- methods.toList
          .traverse(f => bind(FFuncDecl(modifySignature(f.sig), f.exprs)))
          .map(_.flatten)
      } yield typeClassInstanceBind +: methodBinds
    case _ =>
      DesugarError.format(DeclarationNotSupportedDesugarError(d.info))
  }

  def bindTypeAbb(i: String, t: Type): StateEither[Bind] =
    EitherT.liftF(Context.addName(i).map(Bind(_, TypeAbbBind(t))))

  def bindTermAbb(i: String, t: Term): StateEither[Bind] =
    EitherT.liftF(Context.addName(i).map(Bind(_, TermAbbBind(t))))

  def bindTypeClass(i: String, tp: Option[Seq[FTypeParam]]): StateEither[Bind] =
    EitherT.liftF(Context.addName(i).map(Bind(_, TypeClassBind(toKind(tp)))))

  def bindTypeClassInstance(
      typeClass: String,
      typeName: String,
      ty: Type,
      methods: List[String]
  ): StateEither[Bind] =
    EitherT.liftF(
      Context
        .addName(toTypeInstanceBindID(typeClass, typeName))
        .map(Bind(_, TypeClassInstanceBind(typeClass, ty, methods)))
    )

  // # Bind # region_end

  // # Term # region_start

  def buildFunc(
      tp: FTypeParamClause,
      sig: FFuncSig,
      exprs: Seq[FExpr]
  ): StateEither[Term] = for {
    t <- withTermTypeAbs(sig.info, tp)
    abs <- withFuncAbs(sig, toTermExpr(exprs.toList))
  } yield t(abs)

  def withFuncAbs(
      sig: FFuncSig,
      body: StateEither[Term]
  ): StateEither[Term] = sig.p match {
    case Some(params) =>
      val func = params.zipWithIndex
        .foldRight(body) { case ((p, index), acc) =>
          for {
            typ <- toType(p.t)
            variable <- EitherT.liftF(Context.addName(p.i.value))
            // NOTE: The return type specified by the function signature
            // should be added to the last nested abstraction, since that one
            // is returning the value for the function.
            returnType <-
              if (index == params.length - 1) toType(sig.r).map(Some(_))
              else None.pure[StateEither]
            term <- acc
          } yield TermAbs(sig.info, variable, typ, term, returnType)
        }
      withFixCombinator(
        sig.info,
        sig.i.value,
        params.map(_.t).toList,
        sig.r,
        func
      )
    case None =>
      for {
        _ <- EitherT.liftF(Context.addName(WildcardName))
        returnType <- toType(sig.r)
        term <- body
      } yield TermAbs(
        sig.info,
        WildcardName,
        TypeUnit(sig.info),
        term,
        Some(returnType)
      )
  }

  def toTermExpr(
      exprs: List[FExpr],
      letVariable: Option[String] = None
  ): StateEither[Term] = exprs match {
    case Nil => EitherT.rightT(TermUnit(UnknownInfo))
    case l @ FLetExpr(info, i, t, e) :: lexprs =>
      for {
        lt <- withTermLet(info, i.value, t, e.toList)
        le <- toTermExpr(lexprs)
      } yield lt(le)
    case h :: Nil  => toTerm(h, letVariable)
    case expr :: t =>
      // NOTE: When an expression isn't assigned to a variable (through let
      // binding) we are implicitly wrapping it to a let expression that's a
      // no-op. This is primarly to allow side-effects as they are not
      // returning a value .e.g. ``print`` function.
      toTermExpr(FLetExpr(expr.info, FIdentifier("_"), None, List(expr)) :: t)
  }

  def withTermLet(
      info: Info,
      i: String,
      t: Option[FType],
      expr: List[FExpr]
  ): StateEither[Term => Term] =
    for {
      t1 <- toTermExpr(expr, Some(i))
      v <- EitherT.liftF(Context.addName(i))
    } yield t2 => TermLet(info, v, t1, t2): Term

  def toTerm(
      e: FExpr,
      letVariable: Option[String] = None
  ): StateEither[Term] =
    e match {
      case FApp(info, e, typeArgs, args) =>
        for {
          exprTerm <- toTerm(e)
          typedTerm <- toTermTApp(exprTerm, typeArgs)
          computedTerm <- toTermApp(typedTerm, args)
        } yield computedTerm
      case FMatch(info, e, cases) =>
        for {
          me <- toTerm(e)
          mc <- cases.toList.traverse(c => {
            val exprList = c.e.toList
            c.p.toList.traverse(p => Context.runE(toMatchCase(p, exprList)))
          })
        } yield TermMatch(info, me, mc.flatten)
      case FDo(info, actions) =>
        actions.toList match {
          case actions :+ (last: FInfixExpr) => toFlatExpr(actions, last)
          case _ => DesugarError.format(DoRequiresYieldExprDesugarError(info))
        }
      case FProj(info, expr, projections) => toTermProj(info, expr, projections)
      case FMethodApp(info, proj, typeArgs, args) =>
        for {
          termProj <- toTermProj(info, proj.e, proj.ids.tail)
          methodProj = TermMethodProj(info, termProj, proj.ids.head.value)
          typedTerm <- toTermTApp(methodProj, typeArgs)
          withImplicitSelf = TermApp(info, typedTerm, methodProj.t)
          computedTerm <- toTermApp(withImplicitSelf, args, applyUnit = false)
        } yield computedTerm
      case FAssocApp(info, t, i, typeArgs, args) =>
        for {
          ty <- toTypeVar(t.info, t.value)
          assocProj = TermAssocProj(info, ty, i.value)
          typedTerm <- toTermTApp(assocProj, typeArgs)
          computedTerm <- toTermApp(typedTerm, args)
        } yield computedTerm
      case a: FAbs           => toClosure(a, letVariable)
      case FAddition(i1, i2) =>
        toTermOperator(AddOp.operator, i1, i2)
      case FSubtraction(i1, i2) =>
        toTermOperator(SubOp.operator, i1, i2)
      case FMultiplication(i1, i2) =>
        toTermOperator(MulOp.operator, i1, i2)
      case FDivision(i1, i2) =>
        toTermOperator(DivOp.operator, i1, i2)
      case FModulo(i1, i2) =>
        toTermOperator(ModOp.operator, i1, i2)
      case FEquality(i1, i2) =>
        toTermOperator(EqOp.operator, i1, i2)
      case FNotEquality(i1, i2) =>
        toTermOperator(NotEqOp.operator, i1, i2)
      case FLessThan(i1, i2) =>
        toTermOperator(LessThanOp.operator, i1, i2)
      case FLessThanEqual(i1, i2) =>
        toTermOperator(LessThanEqOp.operator, i1, i2)
      case FGreaterThan(i1, i2) =>
        toTermOperator(GreaterThanOp.operator, i1, i2)
      case FGreaterThanEqual(i1, i2) =>
        toTermOperator(GreaterThanEqOp.operator, i1, i2)
      case FAnd(i1, i2) =>
        toTermOperator(AndOp.operator, i1, i2)
      case FOr(i1, i2) =>
        toTermOperator(OrOp.operator, i1, i2)
      case FVar(info, i) =>
        EitherT
          .liftF(toTermVar(info, i))
          .flatMap(_ match {
            case Some(v) => v.pure[StateEither]
            case None    =>
              DesugarError.format(VariableNotFoundDesugarError(info, i))
          })
      case FBool(info, true)  => EitherT.rightT(TermTrue(info))
      case FBool(info, false) => EitherT.rightT(TermFalse(info))
      case FInt(info, i)      => EitherT.rightT(TermInt(info, i))
      case FFloat(info, f)    => EitherT.rightT(TermFloat(info, f))
      case FString(info, s)   => EitherT.rightT(TermString(info, s))
      case FUnit(info)        => EitherT.rightT(TermUnit(info))
      case _                  =>
        DesugarError.format(ExpressionNotSupportedDesugarError(e.info))
    }

  def toClosure(abs: FAbs, letVariable: Option[String]): StateEither[Term] =
    Context.runE(abs match {
      case FAbs(info, bindings, Some(rType), expr) =>
        letVariable match {
          case Some(f) =>
            withFixCombinator(
              info,
              f,
              bindings.map(_.t.get).toList,
              rType,
              withClosure(info, bindings.toList, toTermExpr(expr.toList))
            )
          case _ =>
            withClosure(info, bindings.toList, toTermExpr(expr.toList))
        }
      case FAbs(info, bindings, _, expr) =>
        letVariable match {
          case Some(f) =>
            val letBinding = FBinding(info, FIdentifier(toRecAbsId(f)))
            withClosure(
              info,
              (letBinding :: bindings.toList),
              toTermExpr(expr.toList)
            ).map(TermFix(info, _))
          case _ => withClosure(info, bindings.toList, toTermExpr(expr.toList))
        }
    })

  def toTermProj(info: Info, e: FInfixExpr, ids: Seq[FVar]): StateEither[Term] =
    toTerm(e).map(t =>
      ids.foldLeft(t)((terms, proj) => TermProj(info, terms, proj.value))
    )

  def withClosure(
      info: Info,
      params: List[FBinding],
      body: StateEither[Term]
  ): StateEither[Term] =
    params.foldRight(body) {
      case (FBinding(_, i, Some(t)), acc) =>
        for {
          typ <- toType(t)
          v <- EitherT.liftF(Context.addName(i.value))
          term <- acc
        } yield TermClosure(info, v, Some(typ), term)
      case (FBinding(info, i, _), acc) =>
        for {
          v <- EitherT.liftF(Context.addName(i.value))
          term <- acc
        } yield TermClosure(info, v, None, term)
    }

  def withFixCombinator(
      info: Info,
      name: String,
      params: List[FType],
      returnType: FType,
      func: StateEither[Term]
  ): StateEither[Term] = for {
    ty <- toType(FFuncType(info, params, returnType))
    // The implicit param is added to represent the function we abstract,
    // in order to provide possibility for recursive call. Note that the
    // abstraction is wrapped in a fix combinator.
    variable <- EitherT.liftF(Context.addName(toRecAbsId(name)))
    term <- func
    // NOTE: The abstraction is wrapped with a fix combinator to implement
    // recursion.
  } yield TermFix(info, TermAbs(info, variable, ty, term))

  def toTermTApp(term: Term, typeArgs: FTypeArguments): StateEither[Term] =
    typeArgs
      .getOrElse(Seq())
      .toList
      .traverse(toType(_))
      .map(
        _.foldLeft(term)((term, ty) => TermTApp(term.info, term, ty))
      )

  def toTermApp(
      term: Term,
      args: Seq[FArguments],
      applyUnit: Boolean = true
  ): StateEither[Term] =
    args.toList.sequence.map(_.flatten) match {
      case Some(l) =>
        l.traverse(toTerm(_))
          .map(
            _.foldLeft(term)((term, arg) => TermApp(term.info, term, arg))
          )
      case None if applyUnit =>
        TermApp(term.info, term, TermUnit(term.info))
          .pure[StateEither]
      case _ => term.pure[StateEither]
    }

  def toTermOperator(
      func: String,
      e1: FExpr,
      e2: FExpr
  ): StateEither[Term] = for {
    optionFuncVar <- EitherT.liftF(toTermVar(e1.info, func))
    funcVar <- optionFuncVar match {
      case Some(v) => v.pure[StateEither]
      case None    =>
        DesugarError.format(
          FunctionOperatorNotFoundDesugarError(e1.info, func)
        )
    }
    t1 <- toTerm(e1)
    t2 <- toTerm(e2)
  } yield TermApp(e2.info, TermApp(e1.info, funcVar, t1), t2)

  def toMatchCase(
      p: FPattern,
      e: List[FExpr]
  ): StateEither[(Pattern, Term)] = for {
    p <- toPattern(p)
    ce <- toTermExpr(e)
  } yield (p, ce)

  def toPattern(p: FPattern): StateEither[Pattern] = p match {
    case FIdentifierPattern(info, v, _) => EitherT.rightT(PatternNode(info, v))
    case FVariantOrRecordPattern(info, t, ps) =>
      for {
        np <- ps.toList.traverse(toPattern(_))
        vars <- np.traverse(_ match {
          case PatternNode(info, v, List()) =>
            EitherT.liftF[ContextState, Error, String](Context.addName(v))
          case PatternDefault(_) =>
            EitherT.liftF[ContextState, Error, String](
              Context.addName(WildcardName)
            )
          case _ =>
            // TODO: Use the info from the pattern core class, instead of root pattern.
            DesugarError.format[String](
              NestedPatternNotSupportedDesugarError(p.info)
            )
        })
      } yield PatternNode(info, t.value, vars)
    case FWildCardPattern(info) => EitherT.rightT(PatternDefault(info))
    case FBool(info, true)      => EitherT.rightT(TermTrue(info))
    case FBool(info, false)     => EitherT.rightT(TermFalse(info))
    case FInt(info, i)          => EitherT.rightT(TermInt(info, i))
    case FFloat(info, f)        => EitherT.rightT(TermFloat(info, f))
    case FString(info, s)       => EitherT.rightT(TermString(info, s))
    case FUnit(info)            => EitherT.rightT(TermUnit(info))
    case _ => DesugarError.format(CaseNotSupportedDesugarError(p.info))
  }

  def toFlatExpr(actions: List[FDoAction], body: FExpr): StateEither[Term] =
    actions match {
      case (h: FAssign) :: Nil      => toMap(h, body)
      case (a: FAssign) :: lactions =>
        for {
          ft <- withFlatMap(a)
          fe <- toFlatExpr(lactions, body)
        } yield ft(fe)
      case (h: FInfixExpr) :: _ =>
        DesugarError.format(DoExpectsAssignmentDesugarError((h: FExpr).info))
      case Nil =>
        DesugarError.format(DoExpectsAssignmentDesugarError(body.info))
    }

  def withFlatMap(assign: FAssign): StateEither[Term => Term] = for {
    t1 <- toTermExpr(assign.e.toList)
    v <- EitherT.liftF(Context.addName(assign.i.value))
  } yield t2 => toMethodAppWithClosureArg(t1, FlatMapMethod, v, assign.info, t2)

  def toMap(assign: FAssign, expr: FExpr): StateEither[Term] = for {
    t1 <- toTermExpr(assign.e.toList)
    v <- EitherT.liftF(Context.addName(assign.i.value))
    t2 <- toTerm(expr)
  } yield toMethodAppWithClosureArg(t1, MapMethod, v, assign.info, t2)

  def toMethodAppWithClosureArg(
      t1: Term,
      method: String,
      varName: String,
      varInfo: Info,
      t2: Term
  ) =
    TermApp(
      t1.info,
      TermApp(t1.info, TermMethodProj(t1.info, t1, method), t1),
      TermClosure(varInfo, varName, None, t2)
    )

  def toClassMethod(
      sig: FFuncSig,
      traitTypeParams: Option[Seq[FTypeParam]],
      cls: TypeClass
  ): StateEither[Term] = for {
    typeParams <- sig.tp.getOrElse(Seq()).traverse { v =>
      for {
        id <- EitherT.liftF(Context.addName(v.i.value))
        cls <- v.bounds
          .getOrElse(Seq())
          .traverse(toTypeClass(_))
          .map(_.toList)
      } yield (v.info, id, cls)
    }
    _ <- EitherT.liftF(
      sig.tp.getOrElse(Seq()).traverse(p => Context.addName(p.i.value))
    )
    paramTypes = sig.p.map(_.map(_.t)).getOrElse(Seq())
    funcType <- toType(FFuncType(sig.info, paramTypes, sig.r))
    methodType = typeParams.foldRight(funcType)((p, acc) =>
      // NOTE: In case of Self type parameter we gotta compute its kind
      // based on the type parameters provided for the trait.
      val kind = p._2 match {
        case SelfTypeName => toKind(traitTypeParams)
        case _            => KindStar
      }
      TypeAll(p._1, p._2, kind, p._3, acc)
    )
  } yield TermClassMethod(sig.info, methodType, cls)

  def toParamsWithSelf(
      params: Option[FParamsWithSelf],
      selfTypeIdentifier: FIdentifier,
      selfTypeParams: FTypeParamClause,
      functionTypeParams: FTypeParamClause
  ): Tuple2[FTypeParamClause, Option[FParams]] = {
    params match {
      case Some(FParamsWithSelf(Some(FSelfParam(info)), args)) =>
        val selfParam = FParam(
          info,
          FIdentifier(SelfIdentifier),
          FSimpleType(
            info,
            selfTypeIdentifier,
            Some(
              selfTypeParams
                .getOrElse(Seq())
                .map(tp => FSimpleType(info, FIdentifier(tp.i.value)))
            )
          )
        )
        val typeParams =
          selfTypeParams.toSeq.flatten ++ functionTypeParams.toSeq.flatten
        val combinedParams = selfParam +: args.toSeq.flatten
        (Some(typeParams), Some(combinedParams))
      case Some(FParamsWithSelf(None, Some(params))) =>
        (functionTypeParams, Some(params))
      case _ => (functionTypeParams, None)
    }
  }

  def toTermVar(info: Info, i: String): ContextState[Option[Term]] =
    State { ctx =>
      Context
        .nameToIndex(ctx, toRecordConstructorID(i))
        .orElse(Context.nameToIndex(ctx, i))
        .orElse(Context.nameToIndex(ctx, toRecAbsId(i))) match {
        case Some(index) => (ctx, Some(TermVar(info, index, ctx._1.length)))
        case None        => (ctx, None)
      }
    }

  def toRecAbsId(i: String): String = s"$RecursiveFunctionParamPrefix$i"

  // # Term # region_end

  // # Type # region_start

  def toTypeVariant(info: Info, v: Seq[FVariantTypeValue]): StateEither[Type] =
    v.toList
      .traverse(_ match {
        case FVariantTypeValue(info, FIdentifier(ti), None) =>
          ((ti, TypeUnit(info): Type)).pure[StateEither]
        case FVariantTypeValue(info, FIdentifier(ti), Some(Right(ts))) =>
          toTupleTypeRecord(info, ts).map((ti, _))
        case FVariantTypeValue(info, FIdentifier(ti), Some(Left(p))) =>
          toTypeRecord(info, p).map((ti, _))
      })
      .map(TypeVariant(info, _))

  def toTypeRecord(info: Info, p: FParams): StateEither[Type] =
    p.toList
      .traverse(v => toType(v.t).map((v.i.value, _)))
      .map(TypeRecord(info, _))

  def toTupleTypeRecord(info: Info, ts: FTypes): StateEither[Type] =
    ts.toList.zipWithIndex
      .traverse(v => toType(v._1).map(((v._2 + 1).toString, _)))
      .map(TypeRecord(info, _))

  def withTypeAbs(
      tp: FTypeParamClause,
      typ: StateEither[Type]
  ): StateEither[Type] = tp match {
    case Some(params) =>
      for {
        _ <- EitherT.liftF(params.map(_.i.value).traverse(Context.addName(_)))
        ty <- typ
      } yield params.foldRight(ty) {
        case (FTypeParam(info, FIdentifier(p), _), acc) => TypeAbs(info, p, acc)
      }
    case None => typ
  }

  def withRecType(
      info: Info,
      i: String,
      tp: FTypeParamClause,
      typ: StateEither[Type]
  ): StateEither[Type] = for {
    ri <- EitherT.liftF(Context.addName(toRecId(i)))
    t <- typ
  } yield TypeRec(info, ri, toKind(tp), t)

  // # Type # region_end

  // # Constructors # region_start

  def buildVariantConstructor(
      info: Info,
      variantName: String,
      tp: FTypeParamClause,
      v: FVariantTypeValue
  ): StateEither[Term] = v match {
    case FVariantTypeValue(info, FIdentifier(i), None) =>
      for {
        g <- withTermTypeAbs(info, tp)
        tag = toTermTag(
          info,
          variantName,
          tp,
          i,
          EitherT.rightT(TermUnit(info))
        )
        fold <- withFold(info, variantName, tp, tag)
      } yield g(fold)
    case FVariantTypeValue(info, FIdentifier(i), Some(fields)) =>
      for {
        g <- withTermTypeAbs(info, tp)
        values = toRecordValues(fields)
        r = EitherT.liftF[ContextState, Error, Term](toTermRecord(info, values))
        tag = toTermTag(info, variantName, tp, i, r)
        fold = withFold(info, variantName, tp, tag)
        abs <- withRecordAbs(info, values, fold)
      } yield g(abs)
  }

  def buildRecordConstructor(
      info: Info,
      recordName: String,
      tp: FTypeParamClause,
      fields: Either[FParams, FTypes]
  ): StateEither[Term] = for {
    g <- withTermTypeAbs(info, tp)
    values = toRecordValues(fields)
    r = EitherT.liftF[ContextState, Error, Term](toTermRecord(info, values))
    fold = withFold(info, recordName, tp, r)
    abs <- withRecordAbs(info, values, fold)
  } yield g(abs)

  def toTermRecord(
      info: Info,
      values: List[(String, FType)]
  ): ContextState[Term] =
    values
      .traverse { case (n, t) =>
        toTermVar(info, withTupleParamID(n)).map(term => (n, term.get))
      }
      .map(TermRecord(info, _))

  def toTermTag(
      info: Info,
      name: String,
      tp: FTypeParamClause,
      tag: String,
      body: StateEither[Term]
  ): StateEither[Term] = for {
    term <- body
    typ <- withTypeApp(info, name, tp)
  } yield TermTag(info, tag, term, typ)

  def toRecordValues(fields: Either[FParams, FTypes]) =
    fields match {
      case Left(params) => params.toList.map(f => (f.i.value, f.t))
      case Right(types) =>
        types.toList.zipWithIndex.map(v => ((v._2 + 1).toString, v._1))
    }

  def withTermTypeAbs(
      info: Info,
      typ: FTypeParamClause
  ): StateEither[Term => Term] =
    typ match {
      case Some(p) =>
        p.traverse { v =>
          for {
            id <- EitherT.liftF(Context.addName(v.i.value))
            cls <- v.bounds
              .getOrElse(Seq())
              .traverse(toTypeClass(_))
              .map(_.toList)
          } yield (id, cls)
        }.map(values =>
          (t: Term) =>
            values.foldRight(t)((i, acc) => TermTAbs(info, i._1, i._2, acc))
        )
      case None => EitherT.pure(identity)
    }

  def withRecordAbs(
      info: Info,
      values: List[(String, FType)],
      body: StateEither[Term]
  ): StateEither[Term] =
    values
      .map { case (i, t) => (withTupleParamID(i), t) }
      .foldRight(body)((p, acc) =>
        for {
          typ <- toType(p._2)
          v <- EitherT.liftF(Context.addName(p._1))
          term <- acc
        } yield TermAbs(info, v, typ, term)
      )

  def withFold(
      info: Info,
      name: String,
      tp: FTypeParamClause,
      body: StateEither[Term]
  ): StateEither[Term] =
    for {
      typ <- withTypeApp(info, name, tp)
      term <- body
    } yield TermApp(info, TermFold(info, typ), term)

  def withTypeApp(
      info: Info,
      name: String,
      tp: FTypeParamClause
  ): StateEither[Type] =
    tp match {
      case None      => toTypeVar(info, name)
      case Some(tys) =>
        tys
          .foldLeft(toTypeVar(info, name))((acc, tp) =>
            for {
              typ <- toTypeVar(tp.info, tp.i.value)
              app <- acc
            } yield TypeApp(tp.info, app, typ)
          )
    }

  // Prepends the identifier with "t" if it's an integer – depicting it's used
  // for the tuple records. If not, the identifier is returned unchanged.
  def withTupleParamID(i: String) = i.toIntOption match {
    case Some(v) => s"t$i"
    case None    => i
  }

  // The record constructor has a prefix "%" that should be searched during
  // type checking when the record type is found in the application.
  def toRecordConstructorID(i: String) = s"$RecordConstrPrefix$i"

  // The method type has a prefix "!" that should be searched during type
  // checking when the projection is found for a variable. There's also a "#"
  // separator that depicts the type name for the method.
  def toMethodID(methodName: String, typeName: String) =
    s"$MethodNamePrefix$methodName#$typeName"

  // The method type has a prefix "!" that should be searched during type
  // checking when the projection is found for a variable. There's also a "#"
  // separator that depicts the type name and the type class for the method.
  def toTypeInstanceMethodID(
      methodName: String,
      typeName: String,
      typeClass: String
  ) =
    s"$MethodNamePrefix$methodName#$typeName#$typeClass"

  // The type instance bind has a prefix "#" that should be searched during type
  // checking; constisting of type name and type class name.
  def toTypeInstanceBindID(typeClass: String, typeName: String) =
    s"#$typeClass#$typeName"

  // # Constructors # region_end

  def toType(t: FType): StateEither[Type] =
    t match {
      case FUnitType(info) => EitherT.rightT(TypeUnit(info))
      case FSimpleType(info, FIdentifier("i32"), None) =>
        EitherT.rightT(TypeInt(info))
      case FSimpleType(info, FIdentifier("f32"), None) =>
        EitherT.rightT(TypeFloat(info))
      case FSimpleType(info, FIdentifier("bool"), None) =>
        EitherT.rightT(TypeBool(info))
      case FSimpleType(info, FIdentifier("str"), None) =>
        EitherT.rightT(TypeString(info))
      case FSimpleType(info, FIdentifier(i), None) =>
        toTypeVar(info, i)
      case FSimpleType(info, FIdentifier(i), Some(tys)) =>
        tys.foldLeft((toTypeVar(info, i)))((acc, typ) =>
          for {
            t <- toType(typ)
            app <- acc
          } yield (TypeApp(info, app, t): Type)
        )
      case FFuncType(info, ts, t1) =>
        for {
          i <- ts.toList.traverse(toType(_))
          o <- toType(t1)
          t = i match {
            case Nil => TypeArrow(info, TypeUnit(info), o)
            // Type abstractions (arrow) should be right associative.
            case _ => (i :+ o).reduceRight(TypeArrow(info, _, _))
          }
        } yield t
      case _ => DesugarError.format(TypeNotSupportedDesugarError(t.info))
    }

  def toTypeVar(info: Info, i: String): StateEither[Type] = for {
    value <- EitherT.liftF(State.inspect { (ctx: Context) =>
      (ctx, toCtxIndex(ctx, i))
    })
    typeVar <- value match {
      case (ctx, Some(index)) =>
        TypeVar(info, index, ctx._1.length).pure[StateEither]
      case _ => DesugarError.format(TypeVariableNotFoundDesugarError(info, i))
    }
  } yield typeVar

  def toTypeClass(t: FType): StateEither[TypeClass] = t match {
    case FSimpleType(info, FIdentifier(i), None) => TypeClass(info, i).pure
    case _ => DesugarError.format(TypeBoundNotSupportedDesugarError(t.info))
  }

  def toKind(tp: Option[Seq[FTypeParam]]) =
    List
      .fill(tp.map(_.length).getOrElse(0) + 1)(KindStar: Kind)
      .reduceRight(KindArrow(_, _))

  def toCtxIndex(ctx: Context, i: String): Option[Int] =
    Context.nameToIndex(ctx, i).orElse(Context.nameToIndex(ctx, toRecId(i)))

  def toRecId(i: String) = s"@$i"
}
