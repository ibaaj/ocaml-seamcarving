open Graphics ;;
open Sys ;;

let infini = 100000.0 in

let color_to_rgb c = (c asr 16, (c asr 8) land 255, c land 255) in

let height m = Array.length m in

let width m = Array.length (m.(0)) in

let draw m = Graphics.draw_image (Graphics.make_image m) 0 0 in

let close () = Graphics.close_graph () in

let open_graph m = Graphics.open_graph (" " ^ string_of_int (width m) ^
                                        "x" ^ string_of_int (height m)) in

let is_in i j m = 0 <= i && i < height m && 0 <= j && j < width m in

let dirs = [| (-1,0) ; (0,1) ; (1,0) ; (0,-1) ;
              (-1,-1) ; (1,-1) ; (-1,1) ; (1,1) |] in

let lum (r, g, b) = 0.2126 *. float_of_int r
                 +. 0.7152 *. float_of_int g
                 +. 0.0722 *. float_of_int b in

let dlum p p' = abs_float (lum p -. lum p') in

let drgb (r, g, b) (r', g', b') =
  (*sqrt ( *)(float_of_int r -. float_of_int r') ** 2.0
     +. (float_of_int g -. float_of_int g') ** 2.0
     +. (float_of_int b -. float_of_int b') ** 2.0(**) in

let dist_voisin f x i j m =
  let n = ref 0 in
  let d = ref 0.0 in
  for dir = 0 to x - 1 do
    let k = i + fst dirs.(dir) and l = j + snd dirs.(dir) in
    if is_in k l m then begin
      n := succ !n;
      let p = color_to_rgb m.(i).(j) and p' = color_to_rgb m.(k).(l) in
      d := !d +. f p p';
    end;
  done;
  !d /. float_of_int !n in

let trouver_chemin f x m e =
  let h = height m in
  let w = width m in
  let energie = Array.make_matrix h w 0.0 in
  for i = 0 to h - 1 do
    for j = 0 to w - 1 do
      energie.(i).(j) <- if e.(i).(j) = 0 then dist_voisin f x i j m
                         else if e.(i).(j) = 1 then infini
                         else -.infini;
    done;
  done;
  let voisin i j =
    let r = ref j in
    for k = j - 1 to j + 1 do
      if is_in i k m && energie.(i).(k) < energie.(i).(!r) then
        r := k;
    done;
    !r in
  for i = h - 2 downto 0 do
    for j = 0 to w - 1 do
      energie.(i).(j) <- energie.(i).(j) +. energie.(i + 1).(voisin (i + 1) j);
    done;
  done;
  let chemin = Array.make h 0 in
  for j = 1 to w - 1 do
    if energie.(0).(j) < energie.(0).(chemin.(0)) then
      chemin.(0) <- j;
  done;
  for i = 1 to h - 1 do
    chemin.(i) <- voisin i (chemin.(i - 1));
  done;
  chemin in

let suppression_chemin m chemin =
  let h = height m in
  let w = width m in
  let n = Array.make_matrix h (w - 1) 0 in
  for i = 0 to h - 1 do
    for j = 0 to w - 2 do
      n.(i).(j) <- m.(i).(j + if j >= chemin.(i) then 1 else 0);
    done;
  done;
  n in

let imprimer_chemin m chemin =
  Array.iteri (fun i j -> m.(i).(j) <- Graphics.red) chemin in

let dist_choisie () =
  if Array.length Sys.argv > 3 && String.length Sys.argv.(3) = 4
     && String.sub Sys.argv.(3) 0 3 = "rgb" then drgb
  else dlum
in

let nb_voisins_choisi () =
  if Array.length Sys.argv > 3 && String.length Sys.argv.(3) = 4
     && String.sub Sys.argv.(3) 3 1 = "8" then 8
     else 4
in

let nom_entree_choisi () =
  if Array.length Sys.argv > 1 then Sys.argv.(1)
  else "in.ppm"
in

let nom_sortie_choisi () =
  if Array.length Sys.argv > 2 then Sys.argv.(2)
  else "out.ppm"
in

let sauvegarder m c = if c = 's' then Ppm.save (nom_sortie_choisi ()) m in

let pause m c = if c = ' ' then sauvegarder m (read_key ()) in

let wait x =
  let debut_timer = Sys.time () in
  while Sys.time () -. debut_timer < x do () done
in

let imprimer_rectangle m x y h w c =
  for i = y to y + h - 1 do
    for j = x to x + w do
      if is_in i j m then
        m.(i).(j) <- if c then 1 else -1;
    done;
  done;
in

let imprimer_rectangle_image m x y h w c =
  for i = y to y + h - 1 do
    for j = x to x + w do
      if is_in i j m then
        m.(i).(j) <- c;
    done;
  done;
in

let dessiner m =
  let h = height m in
  let w = width m in
  let g = Array.make_matrix h w 0 in
  for i = 0 to h - 1 do
    g.(i) <- Array.copy m.(i);
  done;
  let e = Array.make_matrix h w 0 in
  let plus = ref true in
  while not (key_pressed ())
        || (let k = read_key () in
            if k = '+' then (plus := true; true)
            else if k = '-' then (plus := false; true)
            else false) do
    draw g;
    let (x,y) = mouse_pos () in
    fill_rect (x - 3) (y - 3) 6 6;
    if button_down () then begin
      imprimer_rectangle e (x - 3) (h - y - 3) 6 6 (!plus);
      imprimer_rectangle_image g (x - 3) (h - y - 3) 6 6
      (if !plus then Graphics.green else Graphics.red);
    end;
  done;
  e in

let img = ref (Ppm.load (nom_entree_choisi ())) in
let f = dist_choisie () in
let x = nb_voisins_choisi () in
open_graph !img;
draw !img;
let e = ref (dessiner !img) in
while width !img > 1 do
  let chemin = trouver_chemin f x (!img) (!e) in
  imprimer_chemin (!img) chemin;
  draw (!img);
  img := suppression_chemin (!img) chemin;
  e := suppression_chemin (!e) chemin;
  wait 0.1;
  clear_graph ();
  draw (!img);
  if key_pressed () then begin
      let c = read_key () in
      sauvegarder (!img) c;
      pause (!img) c;
    end;
done;
close ();
