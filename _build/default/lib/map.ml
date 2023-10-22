type t = string

let create_map : t =
  "\n\
  \                                                      _/`-_-_   _    _\n\
  \                                           __    _-_/       `--` `-_/ `-_\n\
  \   _-__---_--_--_-_               __-_--_-_/  `-_/                     /--`\n\
  \  /               /              /             _/                      `_\n\
  \  `   North       _`         /--`  Europe    _/         Asia            /\n\
  \   `   America   /        _-`              /`                        _  `_\n\
  \    `           /      /``` _/``-_      _-`                         / `-_/\n\
  \     `      _-_/       `-`-'      |    /_        _     /`-_       _/\n\
  \      `    /                       `-_/  `_    _/ `_  /    `-_  _/\n\
  \      /   /                                `-_ /    `-`      /_/`\n\
  \      ` _-                                    `               __ _-_/`-_\n\
  \         _-_-_-_-_                 _-_-_-_-_-__        /`-_/`-` `  __-_/\n\
  \        /         -__--_          /            `-_     `-_  ___  /`\n\
  \        `               `_       /               /        `-`  `-`\n\
  \        `-   South       _|      `-_   Africa   /_             __    |`-_\n\
  \          `-   America   /         -`          _-/            /` `-_-`   `-_\n\
  \           /        _-_/           `-_       _/             /               |\n\
  \           `                          |     `_           _--`  Australia    /\n\
  \            `     /                    `     /           `_               _/\n\
  \             `-_--                     /  __/              `_    _--_    /\n\
  \                                       '-'                   `_-`    `-_-`\n\
  \ "

let display_map (curr_map : t) : unit = print_string curr_map
