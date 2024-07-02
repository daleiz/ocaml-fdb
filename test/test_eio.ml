module F = Fdb.Make (struct
  type +'a t = 'a Eio.Promise.t 

  type 'a u = 'a Eio.Promise.t * 'a Eio.Promise.u

  let read (t, _) = t 

  let fill (_, u) = Eio.Promise.resolve u

  let create () = Eio.Promise.create ~label:"fdb" () 

  let bind t ~f = Eio.Promise.await t |> f

  let map t ~f = Eio.Promise.await t |> f |> Eio.Promise.create_resolved

  let return = Eio.Promise.create_resolved

  let from_future fut =
    let ivar = create () in
    let callback = (fun r ->
      let () = fill ivar r in
      ()
    ) in
    let error = Fdb.Future.set_callback fut callback in 
    match error with
    | Ok () -> read ivar
    | Error _ -> raise (failwith "set_callback error")  
end)

let failwithf = Format.ksprintf (fun s -> failwith s)

let main () =
  let open F.Infix in
  begin
    let key = "foo" in
    F.open_database () >>=? fun db ->
    F.Database.set db ~key ~value:"bar" >>=? fun () ->
    F.Database.get db ~key
  end >>| function
  | Ok (Some value) ->
    if (String.compare value "bar") = 0 then
      print_string "PASS"
    else
      failwithf "FAIL: Expected `bar`, got `%s`" value
  | Ok None -> failwith "FAIL: Failed to fetch key `bar`"
  | Error err -> failwithf "FAIL: FDB error `%s`" (Fdb.Error.to_string err)

let () =
  Eio_main.run @@ fun _ -> main () |> Eio.Promise.await
