module InterpreterStates

open BaseTypes
open System.Collections.Generic

type Session = {
    definedTypes : Dictionary<HstIdentifier, HstType>
    bindings : Dictionary<HstIdentifier, HstBinding>
}

