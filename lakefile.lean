import Lake
open Lake DSL

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
