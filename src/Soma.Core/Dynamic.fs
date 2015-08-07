namespace Soma.Core

open System
open System.Collections
open System.Collections.Concurrent
open System.Collections.Generic
open System.ComponentModel
open System.Data
open System.Data.Common
open System.Dynamic
open System.Runtime.InteropServices
open System.Text
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Text.Lexing

type IDynamicObject =
  inherit IDictionary
  inherit IDictionary<string, obj>
  inherit ICustomTypeDescriptor

  abstract Dialect : IDialect

  abstract GetCaseSensitiveDict : unit -> IDictionary<string, obj>

type dynamic = IDynamicObject

type DynamicObjectPropertyDescriptor(name:string) =
  inherit PropertyDescriptor(name, null)

  override this.ComponentType = typeof<IDynamicObject>

  override this.IsReadOnly = false

  override this.PropertyType = typeof<obj>

  override this.CanResetValue(``component``:obj) = false

  override this.ResetValue(``component``:obj) = ()

  override this.ShouldSerializeValue(``component``:obj) = false

  override this.GetValue(``component``:obj) =
    let dict = ``component`` :?> IDictionary<string, obj>
    dict.[name]

  override this.SetValue(``component``:obj, value:obj) =
    let dict = ``component`` :?> IDictionary<string, obj>
    dict.[name] <- value

type CaseInsensitiveDynamicObject(dialect:IDialect) =
  inherit DynamicObject()

  let mutable propertyDescriptorCollection:PropertyDescriptorCollection = null

  let syncRoot = obj()

  let dynamicMembers = Dictionary<string, obj>(StringComparer.InvariantCultureIgnoreCase) :> IDictionary<string, obj>

  override this.GetDynamicMemberNames() = 
    dynamicMembers.Keys :> seq<string>

  override this.TrySetMember(binder:SetMemberBinder, value) =
    dynamicMembers.[binder.Name] <- value
    true

  override this.TryGetMember(binder:GetMemberBinder, [<Out>] value:obj byref) =
    dynamicMembers.TryGetValue(binder.Name, &value)

  override this.TryInvokeMember(binder:InvokeMemberBinder, args:obj[], [<Out>] result:obj byref) =
    match dynamicMembers.TryGetValue(binder.Name) with
    | true, ``member`` -> 
      match ``member`` with
      | :? Delegate as ``delegate`` ->
        result <- ``delegate``.DynamicInvoke args
        true
      | _ -> false
    | _ -> false

  member this.SyncRoot = syncRoot

  member this.GetCaseSensitiveDict() = 
    Dictionary<string, obj>(dynamicMembers) :> IDictionary<string, obj>

  member this.Dialect = dialect

  member this.Keys = dynamicMembers.Keys

  member this.Values = dynamicMembers.Values

  member this.Count = dynamicMembers.Count
  
  member this.IsReadOnly = dynamicMembers.IsReadOnly
    
  member this.Item 
      with get (key:string) = 
        dynamicMembers.[key]
      and  set (key:string) value = 
        dynamicMembers.[key] <- value
    
  member this.ContainsKey(key:string) = 
    dynamicMembers.ContainsKey(key)
    
  member this.Contains(item:KeyValuePair<string, obj>) = 
    dynamicMembers.Contains(item)
    
  member this.Add(key:string, value:obj) = 
    dynamicMembers.Add(key, value)
    
  member this.Add(item:KeyValuePair<string, obj>) = 
    dynamicMembers.Add(item)
    
  member this.TryGetValue(key:string, [<Out>] value:obj byref) = 
    dynamicMembers.TryGetValue(key, &value)

  member this.CopyTo(array:KeyValuePair<string, obj>[], arrayIndex) = 
    dynamicMembers.CopyTo(array, arrayIndex)
    
  member this.Remove(key:string) = 
    dynamicMembers.Remove(key)

  member this.Remove(item:KeyValuePair<string, obj>) = 
    dynamicMembers.Remove(item)
    
  member this.Clear() = 
    dynamicMembers.Clear()

  member this.GetEnumerator() = 
    dynamicMembers.GetEnumerator()

  member this.GetAttributes() = AttributeCollection(null)

  member this.GetClassName() = null

  member this.GetComponentName() = null

  member this.GetConverter() = null

  member this.GetDefaultEvent() = null

  member this.GetDefaultProperty() = null

  member this.GetEditor(editorBaseType:Type) = null

  member this.GetEvents() = EventDescriptorCollection(null)

  member this.GetEvents(attributes:Attribute[]) = EventDescriptorCollection(null)

  member this.GetProperties() = this.GetProperties(null)

  member this.GetProperties(attributes:Attribute[]) = 
    if propertyDescriptorCollection = null then
      propertyDescriptorCollection <-
        dynamicMembers.Keys
        |> Seq.map (fun key -> DynamicObjectPropertyDescriptor(key) :> PropertyDescriptor)
        |> Seq.toArray
        |> fun props -> PropertyDescriptorCollection(props)
    propertyDescriptorCollection

  member this.GetPropertyOwner(pd:PropertyDescriptor) = box this

  interface IDynamicObject with
    member this.Dialect = this.Dialect
    member this.GetCaseSensitiveDict() = this.GetCaseSensitiveDict()

  interface IDictionary<string, obj> with
    member this.Keys = this.Keys
    member this.Values = this.Values
    member this.Count = this.Count
    member this.IsReadOnly = this.IsReadOnly
    member this.Item
      with get key = this.[key]
      and  set key value = this.[key] <- value
    member this.ContainsKey(key) = this.ContainsKey(key)
    member this.Contains(item) = this.Contains(item)
    member this.Add(key, value) = this.Add(key, value)
    member this.Add(item:KeyValuePair<string, obj>) = this.Add(item)
    member this.TryGetValue(key, [<Out>] value:obj byref) = this.TryGetValue(key, &value)
    member this.CopyTo(array, arrayIndex) = this.CopyTo(array, arrayIndex)
    member this.Remove(key:string) =  this.Remove(key)
    member this.Remove(item:KeyValuePair<string, obj>) = this.Remove(item)
    member this.Clear() = this.Clear()
    member this.GetEnumerator() = this.GetEnumerator()

  interface IDictionary with
    member this.Keys =  ResizeArray(this.Keys) :> ICollection
    member this.Values = ResizeArray(this.Values) :> ICollection
    member this.Count = this.Count
    member this.IsFixedSize = false
    member this.IsReadOnly = this.IsReadOnly
    member this.IsSynchronized = false
    member this.SyncRoot = this.SyncRoot
    member this.Item
      with get key = this.[string key]
      and  set key value = this.[string key] <- value
    member this.Contains(item) = 
      match item with
      | :? KeyValuePair<string, obj> as pair -> this.Contains(pair)
      | _ -> false
    member this.Add(key, value) = this.Add(string key, value)
    member this.CopyTo(array, arrayIndex) =
      let results = Array.zeroCreate<KeyValuePair<string, obj>> array.Length
      this.CopyTo(results, arrayIndex)
      results |> Array.iteri (fun i item -> array.SetValue(item, i))
    member this.Remove(item) = this.Remove(string item) |> ignore
    member this.Clear() = this.Clear()
    member this.GetEnumerator() = 
      let enumerator = this.GetEnumerator()
      { new IDictionaryEnumerator with 
        member this.MoveNext() = enumerator.MoveNext()
        member this.Current = 
          let pair = enumerator.Current
          upcast DictionaryEntry(box pair.Key, box pair.Value)
        member this.Reset() = enumerator.Reset()
        member this.Entry = 
          let pair = enumerator.Current
          DictionaryEntry(box pair.Key, box pair.Value)
        member this.Key = box enumerator.Current.Key
        member this.Value = box enumerator.Current.Value }

  interface IEnumerable with
    member this.GetEnumerator() = this.GetEnumerator() :> IEnumerator

  interface ICustomTypeDescriptor with
    member this.GetAttributes() = this.GetAttributes()
    member this.GetClassName() = this.GetClassName()
    member this.GetComponentName() = this.GetComponentName()
    member this.GetConverter() = this.GetConverter()
    member this.GetDefaultEvent() = this.GetDefaultEvent()
    member this.GetDefaultProperty() = this.GetDefaultProperty()
    member this.GetEditor(editorBaseType:Type) = this.GetEditor(editorBaseType)
    member this.GetEvents() = this.GetEvents()
    member this.GetEvents(attributes:Attribute[]) = this.GetEvents(attributes)
    member this.GetProperties() = this.GetProperties()
    member this.GetProperties(attributes:Attribute[]) = this.GetProperties(attributes)
    member this.GetPropertyOwner(pd:PropertyDescriptor) = this.GetPropertyOwner(pd)
