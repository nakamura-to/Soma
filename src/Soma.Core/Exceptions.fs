namespace Soma.Core

exception NoAffectedRowException of PreparedStatement with
  override this.Message =
    match this :> exn with
    | NoAffectedRowException(ps) -> 
      let message = SR.SOMA4011 (ps.Text, ps.Parameters)
      message.Format()
    | _ -> 
      Unchecked.defaultof<_>

  member this.PreparedStatement =
    match this :> exn with
    | NoAffectedRowException(ps) -> 
      ps
    | _ -> 
      Unchecked.defaultof<_>

exception OptimisticLockException of PreparedStatement with
  override this.Message =
    match this :> exn with
    | OptimisticLockException(ps) -> 
      let message = SR.SOMA4013 (ps.Text, ps.Parameters)
      message.Format()
    | _ -> 
      Unchecked.defaultof<_>

  member this.PreparedStatement =
    match this :> exn with
    | OptimisticLockException(ps) -> 
      ps
    | _ -> 
      Unchecked.defaultof<_>

exception UniqueConstraintException of PreparedStatement * string * exn with
  override this.Message =
    match this :> exn with
    | UniqueConstraintException(ps, message, _) -> 
      let message = SR.SOMA4014 (message, ps.Text, ps.Parameters)
      message.Format()
    | _ -> 
      Unchecked.defaultof<_>

  member this.PreparedStatement =
    match this :> exn with
    | UniqueConstraintException(ps, _, _) -> 
      ps 
    | _ -> 
      Unchecked.defaultof<_>

  member this.Cause =
    match this :> exn with
    | UniqueConstraintException(_, _, cause) -> 
      cause
    | _ -> 
      Unchecked.defaultof<_>

exception EntityNotFoundException of PreparedStatement with
  override this.Message =
    match this :> exn with
    | EntityNotFoundException(ps) -> 
      let message = SR.SOMA4015 (ps.Text, ps.Parameters)
      message.Format()
    | _ -> 
      Unchecked.defaultof<_>

  member this.PreparedStatement =
    match this :> exn with
    | EntityNotFoundException(ps) -> 
      ps
    | _ -> 
      Unchecked.defaultof<_>

type DbException (message:Message, ?innerException:exn) =
  inherit System.InvalidOperationException (message.Format (), match innerException with Some ex -> ex | _ -> null)
  member this.MessageId = message.Id