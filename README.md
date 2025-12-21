[![Website shields.io](https://img.shields.io/website-up-down-green-red/http/shields.io.svg)](https://learnyouahaskell.github.io/)
[![pages-build-deployment](https://github.com/learnyouahaskell/learnyouahaskell.github.io/actions/workflows/pages/pages-build-deployment/badge.svg?branch=main)](https://github.com/learnyouahaskell/learnyouahaskell.github.io/actions/workflows/pages/pages-build-deployment)

**maintainers:** [Stanislav Modrak](https://github.com/smith558), [Artem Pelenitsyn](https://github.com/ulysses4ever), [Pier-Luc Caron St-Pierre](https://github.com/pierluc-codes); to become a maintainer ask in [discussions](https://github.com/learnyouahaskell/learnyouahaskell.github.io/discussions) 🙂

# Learn You a Haskell: A community version

Hey yo! [learnyouahaskell.github.io](https://learnyouahaskell.github.io/) is an **open-source** fork of the original **Learn You a Haskell** (**LYAH** for short) guide for Haskell by Miran Lipovača, "the funkiest way to learn Haskell, the best functional programming language around". This guide is meant for people who have programmed already, but have yet to try functional programming.

**Now, why did I create this fork?** ("fork" means a copy of an original which may be modified or extended)
Well, I began learning Haskell in 2021 at my undergrad studies and quickly came across LYAH ([link for original](https://web.archive.org/web/20251123094006/https://learnyouahaskell.com/)). I immediately fell in love and followed it to supplement my journey of learning Haskell.

However, I quickly realised some parts are slowly becoming outdated as Haskell continues to evolve. That is why, with the author's blessing, I decided to create this open-source fork to enable the Haskell community to participate in preserving and maintaining this awesome resource for the future times.

Anyone is invited to **contribute** by either opening a pull request (preferred) or opening a content edit request (in the pipeline, open soon!) for proposed changes.

The whole thing is completely free to read online, but the original is also available in print and we encourage you to buy a copy!

*This is still a work in progress. Happy for any suggestions or feedback!*

## TODOs:
- [ ] refactor web code
- [ ] make UI more modern (whilst retaining as much of the original "feel" as possible)
- [x] make content pages mobile-friendly (*still needs work)
- [ ] make index page mobile-friendly
- [x] prepare "content edit request" interface on GitHub
- [ ] add exercises
- [ ] update content (just overall, the outdated parts)

For some of these points, there are more focused issues on GitHub.

Don't forget to star the GitHub repository if you like it! 🙂

## Discussion
Discuss the ideas with the community.

* GitHub - Please feel free to use the "Discussions" GitHub space (https://github.com/learnyouahaskell/learnyouahaskell.github.io/discussions)
* Reddit - https://www.reddit.com/r/haskell/comments/sogi3s/learn_you_a_haskell_a_community_version/
* Haskell Discourse - https://discourse.haskell.org/t/learn-you-a-haskell-a-community-version/4056

## Contributing

We are happy to get your contributions!
For the most part, you can simply edit Markdown files and open a PR with the edits.
If you want to preview the changes locally, you need to build the site as discussed below.

### Building the site

The site is built using [Hakyll](https://jaspervdj.be/hakyll/), a static site generator in the form of a Haskell library.
With Hakyll, you first build a Haskell application `site`, and then run it to generate HTML for the website (some workflows combine these steps; notably, `cabal run`).
You can build the application in two ways common for Haskell software: with `cabal` (standard, slow first build) or `nix` (advanced, fast first build).
Both of them rely on the same Haskell package description (the `.cabal` file), so they should produce the same result.

#### Option 1: Using Cabal

You will need the Haskell toolchain, GHC and Cabal, installed (e.g. via [GHCup][ghcup]).
After that you can use `cabal` to build the website as follows:

[ghcup]: https://www.haskell.org/ghcup/

```bash
# Update package list (first time only)
cabal update

# Build and run the site generator
cabal run site -- build

# Preview the site locally (optional)
cabal run site -- watch
# Then visit http://localhost:8000
```

A variation of these commands is used in the `Makefile`: run `make` to generate the website.

#### Option 2: Using Nix

[Nix][nix] is less straightforward to obtain usually, so it's not recommended for newcomers.
Nix provides pre-compiled binary packages for all dependencies, which significantly speeds up the first build.
This is what we use in our GitHub CI for expediency.

[nix]: https://nixos.org/

```bash
# Build the site application binary with Nix
nix-build

# Run the site generator to build the site
result/bin/site build

# Preview the site locally (optional)
result/bin/site watch
# Then visit http://localhost:8000
```

Alternatively, you can use the `Makefile`:
```bash
# Build the binary and generate the site
make nix-site
```

### Build results

No matter how you build the site, it will end up in the `_site/` directory.

## Licence
This domain and repository is in no way affiliated with Miran Lipovača (the original author) and is being extended and modified with his permission as per the licence the original work was released under ([Creative Commons Attribution-Noncommercial-ShareAlike 3.0 Unported License](http://creativecommons.org/licenses/by-nc-sa/3.0/)) as well as his literal statement encouraging modifications to be made ([FAQ](https://web.archive.org/web/20250126151541/http://learnyouahaskell.com/faq)).

##
This work is licensed under a [Creative Commons Attribution-Noncommercial-ShareAlike 3.0 Unported License](http://creativecommons.org/licenses/by-nc-sa/3.0/).
