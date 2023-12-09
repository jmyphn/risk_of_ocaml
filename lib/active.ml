open Raylib

type t = {
  bg : Texture2D.t;
  state : Texture2D.t;
  state_hl : Texture2D.t;
  phase : Texture2D.t;
  phase_hl : Texture2D.t;
}

let active = ref None

let initialize_active () =
  let active_bg = load_texture "assets/active/map.png" in
  let state = load_texture "assets/TempButton.png" in

  let state_hl = load_texture "assets/TempButtonHighlight.png" in
  let phase = load_texture "assets/active/PhaseButton.png" in

  let phase_hl = load_texture "assets/active/PhaseButtonHi.png" in
  active := Some { bg = active_bg; state; state_hl; phase; phase_hl }

let get_hl () =
  let a = Option.get !active in
  (a.state_hl, a.phase_hl)

let draw_active () =
  let a = Option.get !active in
  let sw = float_of_int Constants.screen_width in
  let sh = float_of_int Constants.screen_height in
  let source = Rectangle.create 0. 0. sw sh in
  let dest = Rectangle.create 0. 0. sw sh in
  let origin = Vector2.create 0. 0. in
  draw_texture_pro a.bg source dest origin 0. Constants.default_color;
  draw_texture a.state 300 350 Constants.default_color;
  draw_texture a.phase 750 350 Constants.default_color
