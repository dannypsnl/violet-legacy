import Lake
open Lake DSL

require «parsec-extra» from git "https://git.sr.ht/~dannypsnl/parsec-extra" @ "main"

package «violet» {
  -- add package configuration options here
}

lean_lib «Violet» {
  -- add library configuration options here
}

@[default_target]
lean_exe «violet» {
  root := `Main
}

require «lspec» from git "https://github.com/lurk-lab/LSpec" @ "main"
lean_exe Tests.Parsing
