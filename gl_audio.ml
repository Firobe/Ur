open Gl_utils
open Tsdl_mixer

let channels = 100

let init () =
  let open Mixer.Init in
  let* flags = Mixer.init ogg in
  let* () =
    if test flags ogg then Result.ok ()
    else Result.error (`Msg "Failed to load OGG support")
  in
  let* () =
    Mixer.open_audio Mixer.default_frequency Mixer.default_format
      Mixer.default_channels 1024
  in
  if Mixer.allocate_channels () channels = channels then Result.ok ()
  else Result.error (`Msg "Could not allocate enough channels")

let terminate () = Mixer.close_audio () ; Mixer.quit ()

module Sound = struct
  type t = Mixer.chunk

  let create path volume =
    let f_vol = int_of_float (volume *. float Mixer.max_volume) in
    let* chunk = Mixer.load_wav path in
    ignore (Mixer.volume_chunk chunk f_vol) ;
    Ok chunk

  let delete t = Mixer.free_chunk t

  let play t =
    let* _ = Mixer.play_channel (-1) t 0 in
    Ok ()

  let shut_up () = Mixer.halt_channel (-1)
end

type t =
  { ressources: (Themes.sound_type * Sound.t) list
  ; locks: (Themes.sound_type * Animation.t) list }

let load_theme themes =
  let l = Themes.sounds themes in
  let loaded =
    List.map
      (fun (_, path, vol) ->
        let path = Themes.prepend_path themes path in
        Sound.create path vol )
      l in
  let* sounds = flatten_result_list loaded in
  let ressources = List.map2 (fun (a, _, _) b -> (a, b)) l sounds in
  let locks = [] in
  Result.ok {ressources; locks}

let delete_sounds {ressources; _} =
  List.iter (fun (_, b) -> Sound.delete b) ressources

let play_theme ?anim_unique t kind =
  let play locks =
    let* () =
      match List.assoc_opt kind t.ressources with
      | Some s -> Sound.play s
      | None -> Result.ok ()
    in
    Result.ok {t with locks} in
  match anim_unique with
  | None -> play t.locks
  | Some a -> (
    match List.assoc_opt kind t.locks with
    | None -> play ((kind, a) :: t.locks)
    | Some b when Animation.eq a b -> Result.ok t
    | Some _ ->
        let locks' = List.remove_assoc kind t.locks in
        play ((kind, a) :: locks') )
