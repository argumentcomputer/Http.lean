import Lake
open Lake DSL

package Http {
  srcDir := "src"
  defaultFacet := PackageFacet.staticLib
  dependencies := #[
    {
      name := `socket
      src := Source.git "https://github.com/xubaiw/Socket.lean" "e2b40341017eacd03d006630e6f863ab7c1ede9f"
    }
  ]
}
