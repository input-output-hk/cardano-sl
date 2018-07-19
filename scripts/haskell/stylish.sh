find . -type f -name "*hs" -not -path '.git' -not -path '*.stack-work*' -not -name 'HLint.hs' -exec stylish-haskell -i {} \;
