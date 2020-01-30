class ['a] sliding  ~count = object(self)
  inherit ['a] Types.window

  val mutable window: 'a list = []

  method private slide v =
    let rec drop = function
      | [ ]
      | [_] -> []
      | hd :: tl -> hd :: (drop tl)
    in
    v :: (drop window)

  method content = window

  method process v =
    if (List.length window) = count then
      let result = Some window in
      window <- self#slide v;
      Lwt.return result
    else begin
      window <- v :: window;
      Lwt.return None
    end
end

class ['a] tumbling ~count = object
  inherit ['a] Types.window

  val mutable window: 'a list = []

  method content = window

  method process v =
    if (List.length window) = count then
      let result = Some window in
      window <- [];
      Lwt.return result
    else begin
      window <- v :: window;
      Lwt.return None
    end
end
