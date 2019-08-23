module BaseTypes

type HstAtomicTypeName = TypeName of string
and HstIdentifier = HstIdentifier of string
and HstUnit = HstUnit of unit

and HstType =
    | TypeAlias of string * HstTypeSig
    | RecordType of HstAtomicTypeName * (HstIdentifier * HstType) list
    | SumType of (HstAtomicTypeName * HstType) list

and HstTypeSig =
    | Atomic of HstAtomicTypeName
    | Chain of HstAtomicTypeName list

and HstBinding =
    { identifier : HstIdentifier
      args : HstIdentifier list option
      bindingType : HstTypeSig
      exps : HstStatement list }

and HstIfStmt =
    { cond : HstExpression
      thendo : HstStatement list }

and HstIfElseStmt =
    { cond : HstExpression
      thendo: HstStatement list
      elsedo : HstStatement list }

and HstExpression =
    | ExpValue of HstValue
    | ExpOpCall of HstOperatorCall
    | BindingExpr of HstIdentifier

and HstStatement =
     | IfStmt of HstIfStmt
     | IfElseStmt of HstIfElseStmt
     | TypeDef of HstType
     | Binding of HstBinding
     | Value of HstValue
     | OperatorCall of HstOperatorCall
     | PrintStatement of HstExpression
     | Loop of HstLoop
     | FuncCall of HstFuncCall

and HstFuncCall =
   { id : HstIdentifier
     callArgs : HstCallArg list }

and HstLoop =
    { fromIndex : HstNumber
      toIndex : HstNumber
      boundIdf : HstIdentifier
      body : HstStatement list }

and HstValue =
    | Number of HstNumber
    | String of HstString
    | RecordInstance of HstRecordInstance
    | Boolean of HstBoolean
    | Unit

and HstNumber =
    | Float of float
    | Integer of int

and HstCallArg =
    | CallValue of HstValue
    | CallIdf of HstIdentifier

and HstString = string

and HstRecordInstance = HstAtomicTypeName * Map<HstIdentifier, HstValue>

and HstBoolean = HstTrue | HstFalse

and HstOperator = Operator of HstString

and HstOperatorCall =
    { op : HstOperator
      exp1 : HstExpression
      exp2 : HstExpression }