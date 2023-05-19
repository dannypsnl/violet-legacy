import Lake
open Lake DSL

require «parsec-extra» from git "https://github.com/dannypsnl/parsec-extra" @ "main"

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
