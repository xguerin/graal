class ['a, 'b] sliding ~(fn: ('a list -> 'b)) ~count = object(self)
  val mutable window: 'a list = []

  method private slide v =
    let rec drop = function
      | [ ]
      | [_] -> []
      | hd :: tl -> hd :: (drop tl)
    in
    v :: (drop window)

  method write v =
    if (List.length window) = count then
      let result = Some (fn window) in
      window <- self#slide v;
      Lwt.return result
    else begin
      window <- v :: window;
      Lwt.return None
    end
end

class ['a, 'b] tumbling ~(fn: ('a list -> 'b)) ~count = object
  val mutable window: 'a list = []

  method write v =
    if (List.length window) = count then
      let result = Some (fn window) in
      window <- [];
      Lwt.return result
    else begin
      window <- v :: window;
      Lwt.return None
    end
end
