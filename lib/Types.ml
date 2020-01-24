class ['a] reader = object
  method read: 'a Lwt.t =
    failwith "unimplemented"
end

class ['a] writer = object
  method write (_: 'a): unit Lwt.t =
    failwith "unimplemented"
end

class ['a] stream = object
  inherit ['a] reader
  inherit ['a] writer
end

class operator = object
  method process: unit Lwt.t =
    failwith "unimplemented"
end
