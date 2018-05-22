{ url # The Git revision of nixpkgs to fetch
, ref # The SHA256 of the downloaded data
}:

builtins.fetchGit {
  url = url;
  ref = ref;
}
