load("@bazel_skylib//lib:paths.bzl", "paths")
load(
    "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_cc_configure",
    "nixpkgs_local_repository",
    "nixpkgs_package",
)
load("@rules_haskell//haskell:nixpkgs.bzl", "haskell_register_ghc_nixpkgs")
load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")
load("@rules_haskell//haskell:repositories.bzl", "rules_haskell_dependencies")
load("@rules_haskell//tools:repositories.bzl", "rules_haskell_worker_dependencies")

def setup_haskell():
    # Import external repositories that `rules_haskell` needs to work properly.
    rules_haskell_dependencies()

    # Define a Nixpkgs repository that uses the Haskell pin defined in
    # `nix/haskell`.
    nixpkgs_local_repository(
        name = "haskell_nixpkgs",
        nix_file = "//nix/haskell:default.nix",
    )

    # We need to ensure that the C compiler and associated toolchain used by our
    # Haskell toolchain are pinned and hermetic, so we use
    # `nixpkgs_cc_configure` from `rules_nixpkgs` to pick those from the
    # aforementioned repository's `staticHaskell` package set.
    nixpkgs_cc_configure(
        repository = "@haskell_nixpkgs",

        # `nixpkgs_cc_configure` is expected to specify an attribute, Nix file or
        # inline Nix expression (as used here) that exposes a `stdenv.cc` and
        # compatible `binutils`. We pick both from our `staticHaskell` package
        # set.
        nix_file_content = """
          with import <nixpkgs> { config = {}; overlays = []; }; buildEnv {
            name = "bazel-cc-toolchain";
            paths = [ staticHaskell.stdenv.cc staticHaskell.binutils ];
          }
        """,
    )

    # Pull GHC from the Haskell Nixpkgs pin, which provides version {{ghc_version}}.
    haskell_register_ghc_nixpkgs(
        version = "{{ghc_version}}",
        repository = "@haskell_nixpkgs",
        attribute_path = "staticHaskell.ghc",

        # We are building fully-statically-linked binaries, so our runtime is
        # static and this is a static toolchain. Thus both `static_runtime` and
        # `fully_static_link` must be set to `True` so that `rules_haskell` will
        # take the steps necessary to make static compilation work. If this is
        # omitted or set to false, `-optl-static` in a `haskell_binary` rule
        # will _not_ be sufficient to get everything working.
        static_runtime = True,
        fully_static_link = True,

        # Global compiler flags (here as an example).
        compiler_flags = [
            "-Wall",
        ],
    )

    # Set up build-time dependencies (`stack`, `happy`, etc.)
    setup_build_time_dependencies()

    # Set up system/C-level dependencies (e.g. native PostgreSQL libraries for
    # `postgresql-simple`, `zlib`, etc.)
    setup_system_c_dependencies()

    # Define our Stack snapshot. You can define this either inline (as shown
    # here) or using a `stack.yaml` in your repository (potentially preferable
    # if more of your developers are already familiar with Stack and its file
    # formats).
    stack_snapshot(
        name = "stackage",
        snapshot = "{{stackage_version}}",

        # The `stack` binary is the one we defined in
        # `setup_build_time_dependencies` and is pinned by virtue of coming from
        # Nixpkgs.
        stack = "@haskell_nixpkgs_stack//:bin/stack",

        packages = [
            # Core libraries
            "array",
            "base",
            "bytestring",
            "containers",
            "deepseq",
            "directory",
            "filepath",
            "ghc-heap",
            "mtl",
            "process",
            "text",
            "template-haskell",

            # Hackage dependencies
            "hashable",
            "hslua",
            "postgresql-simple",
            "zlib",
        ],

        # Other build tools that come from either
        # `setup_build_time_dependencies` (e.g. `alex`, `happy`) or
        # `setup_system_c_dependencies` (e.g.  `pg_config`, which is needed to
        # hook up the C-level `libpq` to the Haskell-level `libpq`).
        tools = [
            "@haskell_nixpkgs_alex//:bin/alex",
            "@haskell_nixpkgs_c2hs//:bin/c2hs",
            "@haskell_nixpkgs_postgresql//:bin/pg_config",
            "@haskell_nixpkgs_happy//:bin/happy",
        ],

        # The `extra_deps` attribute allows us to wire up Haskell packages to
        # system/C-level libraries and dependencies that (in our case) are
        # provided by Nixpkgs and defined in `setup_system_c_dependencies`. The
        # format is:
        #
        # ```
        # "haskell-library-name": [
        #   ... list of Bazel targets exposing system/C-level dependencies ...
        # ]
        # ```
        #
        extra_deps = {
            "postgresql-libpq": [
                # Note that as per
                # https://github.com/nh2/static-haskell-nix/issues/57, we need
                # to link against `openssl` whenever we link against `libpq`.
                # Moreover, note the order in which we link `libcrypto` and
                # `libssl` is important -- see "Pulling in OpenSSL from Nixpkgs"
                # for more information.
                #
                # DO NOT REORDER THESE TWO DEPENDENCIES.
                #
                "@haskell_nixpkgs_openssl//:c_lib",
                "@haskell_nixpkgs_crypto//:c_lib",

                "@haskell_nixpkgs_postgresql//:c_lib",
            ],
            "zlib": [
                "@haskell_nixpkgs_zlib//:c_lib",
            ],
        },
    )

    # Set up ghcide for developer experience.
    setup_ghcide()

    # We shouldn't need this but otherwise using `bazel query` may fail.
    # See https://github.com/tweag/rules_haskell/issues/1078
    rules_haskell_worker_dependencies()

def setup_build_time_dependencies():
    # Broadly speaking, build-time dependencies do not need to be built
    # statically. Examples include `stack`, for fetching package sources,
    # `happy` for preprocessing compatible builds, etc. which contribute source
    # code to the build but which have no part themselves in the final
    # executable. For these packages, we pick them from the "vanilla"
    # `haskellPackages` attribute in our repository, which should a. be more
    # likely to work and b. give us better caching.

    nixpkgs_package(
        name = "haskell_nixpkgs_stack",
        repository = "@haskell_nixpkgs",
        attribute_path = "haskellPackages.stack",
    )

    nixpkgs_package(
        name = "haskell_nixpkgs_alex",
        repository = "@haskell_nixpkgs",
        attribute_path = "haskellPackages.alex",
    )

    nixpkgs_package(
        name = "haskell_nixpkgs_c2hs",
        repository = "@haskell_nixpkgs",
        attribute_path = "haskellPackages.c2hs",
    )

    nixpkgs_package(
        name = "haskell_nixpkgs_happy",
        repository = "@haskell_nixpkgs",
        attribute_path = "haskellPackages.happy",
    )

def setup_system_c_dependencies():
    # PostgreSQL
    #
    # PostgreSQL exposes a lot of libraries; we only want `libpq`. We thus make
    # use of the explicit `libs` attribute of `nixpkgs_cc_library_package` to
    # filter out the rest. If we don't do this, we end up with lots of linking
    # errors when Bazel generates link-time arguments referencing these
    # libraries but they haven't been copied to the workspace since the Haskell
    # libraries don't actually use them.
    nixpkgs_cc_library_package(
        name = "haskell_nixpkgs_postgresql",
        repository = "@haskell_nixpkgs",
        attribute_paths = [
            "staticHaskell.postgresql",
            "staticHaskell.postgresql.lib",
        ],
				libs = [
					"lib/libpq.so*",
					"lib/libpq.dylib",
					"lib/libpq.a",
				],
        cc_library = dict(
            name = "c_lib",
            srcs = [":lib"],
            hdrs = [":include"],
            strip_include_prefix = "include",
            visibility = ["//visibility:public"],
            linkstatic = True,
        ),
    )

    # OpenSSL
    #
    # There are a couple of considerations when pulling in OpenSSL from Nixpkgs
    # to make things work:
    #
    # * While both `libcrypto` and `libssl` come from `openssl`, we bring them in
    #   separately. This is because they must be linked in a particular order
    #   (`libssl` must be linked _before_ `libcrypto`) when static linking on `.a`
    #   files is being performed (see e.g.
    #   https://github.com/nutechsoftware/ser2sock/pull/13/files), and having two
    #   separate Bazel targets allows us to achieve this without excessive hacking
    #   (see the order these targets are passed as `extra_deps` to libraries that
    #   need them).
    #
    # * We pull in a custom derivation, `openssl_both`, that includes both
    #   dynamic (`*.so`, etc.) and static (`*.a`) libraries. This is so that
    #   both building (GHC, static dependencies) and REPLs (GHCi, dynamic
    #   dependencies without hacking GHCi) work. This is in the vein of the
    #   `zlib_both` package, which is already provided by `static-haskell-nix`.
    #
    nixpkgs_cc_library_package(
        name = "haskell_nixpkgs_crypto",
        repository = "@haskell_nixpkgs",
        attribute_paths = [
            "staticHaskell.openssl_both.dev",
            "staticHaskell.openssl_both.out",
        ],
        libs = [
          "lib/libcrypto.*",
        ],
        cc_library = dict(
            name = "c_lib",
            srcs = [":lib"],
            hdrs = [":include"],
            strip_include_prefix = "include",
            visibility = ["//visibility:public"],
            linkstatic = True,
        ),
    )

    nixpkgs_cc_library_package(
        name = "haskell_nixpkgs_openssl",
        repository = "@haskell_nixpkgs",
        attribute_paths = [
            "staticHaskell.openssl_both.dev",
            "staticHaskell.openssl_both.out",
        ],
        libs = [
          "lib/libssl.*",
        ],
        cc_library = dict(
            name = "c_lib",
            srcs = [":lib"],
            hdrs = [":include"],
            strip_include_prefix = "include",
            visibility = ["//visibility:public"],
            linkstatic = True,
        ),
    )

    # zlib
    nixpkgs_cc_library_package(
        name = "haskell_nixpkgs_zlib",
        repository = "@haskell_nixpkgs",
        attribute_paths = [
            "staticHaskell.zlib_both.dev",
            "staticHaskell.zlib_both.out",
        ],
        cc_library = dict(
            name = "c_lib",
            srcs = [":lib"],
            hdrs = [":include"],
            strip_include_prefix = "include",
            visibility = ["//visibility:public"],
            linkstatic = True,
        ),
    )

# This is a helper function for pulling in a set of packages from Nixpkgs and
# making their C libraries/header files available as a Bazel target that can be
# used e.g. in `extra_deps` in `stack_snapshot`.
def nixpkgs_cc_library_package(
    name,
    repository,
    cc_library,
    attribute_paths = None,
    libs = None,
    **kwargs):

    if attribute_paths:
        nix_file_content = "\n".join([
            "with (import <nixpkgs> { config = {}; overlays = []; });",
            'buildEnv {name = "%s"; paths = [ %s ]; }' % (name, " ".join(attribute_paths)),
        ])
        kwargs = dict(kwargs, nix_file_content = nix_file_content)

    # If an explicit set of `libs` is given, use those. Otherwise, glob all
    # dynamic and static libraries (including those we might find on
    # Darwin/macOS).
    libs = libs or [paths.join("lib", "**/*") + ext for ext in [".so*", ".dylib", ".a"]]

    build_file_lines = (
        [
            'package(default_visibility = ["//visibility:public"])',
            "",
            "filegroup(",
            '    name = "bin",',
            '    srcs = glob(["bin/*"]),',
            ")",
            "",
            "filegroup(",
            '    name = "lib",',
            "    srcs = glob(%r)" % libs,
            ")",
            "",
            "filegroup(",
            '    name = "include",',
            '    srcs = glob(["include/**/*.h"]),',
            ")",
            "",
            "cc_library(",
        ] +
        ["    %s = %r," % (k, v) for k, v in cc_library.items()] +
        [")"]
    )

    nixpkgs_package(
        name = name,
        repository = repository,
        build_file_content = "\n".join(build_file_lines),
        **kwargs
    )

def setup_ghcide():
    # To ensure that we get a version of ghcide compatible with our compiler, we
    # build ourselves a pinned copy. To do this we define a separate
    # `stack_snapshot` whose only job is to build ghcide. We pin this using a
    # local snapshot derived from the `stack.yaml` found in the pinned version
    # of ghcide's GitHub repository (see `ghcide-stack.yaml` for more
    # information).
    stack_snapshot(
        name = "ghcide",
        local_snapshot = "//bazel/repository:ghcide-stack.yaml",

        # We only want the ghcide package from this snapshot.
        packages = [
            "ghcide",
        ],

        # By default, `stack_snapshot` is geared towards building library
        # targets for Cabal packages in a snapshot. In this case we want an
        # executable target, namely that of the `ghcide` package. The
        # `components` attribute lets us override the targets to be built on a
        # per-package basis, so we use it here to ensure we get the ghcide
        # executable target. This will then be made available under
        # `@ghcide-exe//ghcide`.
        components = {
            "ghcide": [
                "lib",
                "exe",
            ],
        },

        # ghcide's transitive dependency set depends on zlib, so we need to wire
        # that up to our C dependencies as in the main snapshot.
        extra_deps = {
            "zlib": [
                "@haskell_nixpkgs_zlib//:c_lib",
            ],
        },

        # There's no need to build Haddocks for this snapshot, so we disable
        # them to speed up builds.
        haddock = False,
    )
