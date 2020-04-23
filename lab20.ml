open Graphics ;;

type photo = float list list ;;
(* images are lists of lists of floats between 0. (white) and 1. (black) *)
type size = int * int ;;
  
(* threshold thershold image -- image where pixels above the threshold
value are black *)

let imagemod (f : float -> float) (img : photo) : photo = 
  List.map (fun row -> List.map f row) img

let threshold (img : photo) (t : float) : photo = 
  imagemod (fun x -> if x <= t then 0. else 1.) img ;;

let dither (img : photo) : photo = 
  imagemod (fun x -> if x > Random.float 1. then 1. else 0.) img ;;
     
(* show the image *)
let depict (img : photo) : unit =
  open_graph "";
  clear_graph ();

  let w, h = List.length (List.hd img), List.length img in resize_window w h;

  let depict_pix (pixel : float) (r : int) (c : int) = 
    let color = int_of_float (255. *. (1. -. pixel)) in 
      set_color (rgb color color color);
      plot c (h - r) in

  List.iteri (fun r row -> List.iteri (fun c pix -> depict_pix pix r c) row) img;
  Unix.sleep 2; 
  close_graph () ;;

let mona = Monalisa.image ;;
  
let mona_threshold = threshold mona 0.75 ;;  

let mona_dither = dither mona ;;

let _ = 
  depict mona;
  depict mona_threshold;
  depict mona_dither ;;
           
