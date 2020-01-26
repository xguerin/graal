class type ['a] reader = object
  method read: 'a Lwt.t
end

class type ['a] writer = object
  method write: 'a -> unit Lwt.t
end

class type ['a] stream = object
  inherit ['a] reader
  inherit ['a] writer
end

class type operator = object
  method process: unit Lwt.t
end
