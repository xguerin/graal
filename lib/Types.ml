class virtual ['a] reader = object
  method virtual read: 'a Lwt.t
end

class virtual ['a] writer = object
  method virtual write: 'a -> unit Lwt.t
end

class virtual ['a] stream = object
  inherit ['a] reader
  inherit ['a] writer
end

class virtual operator = object
  method virtual process: unit Lwt.t
end

class virtual ['a] window = object
  method virtual content: 'a list
  method virtual process: 'a -> 'a list option Lwt.t
end
