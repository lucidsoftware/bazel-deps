scala_library(
    name = "depsmodel",
    srcs = ["DepsModel.scala"],
    visibility = ["//visibility:public"],
    exports = [
        "//3rdparty/jvm/org/typelevel:cats_core",
        "@org_typelevel_paiges//:paiges",
    ],
    deps = [
        "//3rdparty/jvm/org/eclipse/aether:aether_api",
        "//3rdparty/jvm/org/typelevel:cats_core",
        "//3rdparty/jvm/org/typelevel:cats_kernel",
        "@org_typelevel_paiges//:paiges",
    ],
)

scala_library(
    name = "commands",
    srcs = ["Commands.scala"],
    exports = [
        ":depsmodel",
        "@com_monovore_decline//:decline",
    ],
    deps = [
        ":depsmodel",
        "//3rdparty/jvm/org/eclipse/aether:aether_api",
        "@com_monovore_decline//:decline",
    ],
)

scala_library(
    name = "graph",
    srcs = ["Graph.scala"],
    visibility = ["//visibility:public"],
    exports = ["@org_typelevel_paiges//:paiges"],
    deps = ["@org_typelevel_paiges//:paiges"],
)

scala_repl(
    name = "repl",
    deps = [":graph"],
)

scala_library(
    name = "resolver",
    srcs = ["Resolver.scala"],
    visibility = ["//visibility:public"],
    deps = [
        ":depsmodel",
        ":graph",
        "//3rdparty/jvm/org/apache/maven:maven_aether_provider",
        "//3rdparty/jvm/org/eclipse/aether:aether_api",
        "//3rdparty/jvm/org/eclipse/aether:aether_connector_basic",
        "//3rdparty/jvm/org/eclipse/aether:aether_impl",
        "//3rdparty/jvm/org/eclipse/aether:aether_spi",
        "//3rdparty/jvm/org/eclipse/aether:aether_transport_file",
        "//3rdparty/jvm/org/eclipse/aether:aether_transport_http",
    ],
)

scala_library(
    name = "io",
    srcs = ["IO.scala"],
    visibility = ["//visibility:public"],
    exports = [
        "//3rdparty/jvm/org/typelevel:cats_free",
    ],
    deps = [
        "//3rdparty/jvm/org/typelevel:cats_core",
        "//3rdparty/jvm/org/typelevel:cats_free",
    ],
)

scala_library(
    name = "normalizer",
    srcs = ["Normalizer.scala"],
    visibility = ["//visibility:public"],
    deps = [
        ":depsmodel",
        ":graph",
        "//3rdparty/jvm/org/eclipse/aether:aether_api",
        "@org_typelevel_paiges//:paiges",
    ],
)

scala_library(
    name = "writer",
    srcs = [
        "Label.scala",
        "Target.scala",
        "Writer.scala",
    ],
    visibility = ["//visibility:public"],
    deps = [
        ":depsmodel",
        ":graph",
        ":io",
        "//3rdparty/jvm/org/eclipse/aether:aether_api",
        "//3rdparty/jvm/org/typelevel:cats_core",
        "//3rdparty/jvm/org/typelevel:cats_free",
        "@org_typelevel_paiges//:paiges",
    ],
)

scala_library(
    name = "makedeps",
    srcs = [
        "FormatDeps.scala",
        "MakeDeps.scala",
        "MergeDeps.scala",
    ],
    visibility = ["//visibility:public"],
    deps = [
        ":circeyaml",
        ":commands",
        ":decoders",
        ":depsmodel",
        ":graph",
        ":io",
        ":normalizer",
        ":resolver",
        ":writer",
        "//3rdparty/jvm/io/circe:circe_core",
        "//3rdparty/jvm/io/circe:circe_jawn_2_11",
        "//3rdparty/jvm/org/eclipse/aether:aether_api",
        "//3rdparty/jvm/org/typelevel:cats_core",
        "//3rdparty/jvm/org/typelevel:cats_free",
    ],
)

scala_binary(
    name = "parseproject",
    srcs = ["ParseProject.scala"],
    main_class = "com.github.johnynek.bazel_deps.ParseProject",
    visibility = ["//visibility:public"],
    runtime_deps = [
        "//3rdparty/jvm/org/slf4j:slf4j_simple",
    ],
    deps = [
        ":commands",
        ":makedeps",
    ],
)

scala_library(
    name = "circeyaml",
    srcs = ["CirceYaml.scala"],
    visibility = ["//visibility:public"],
    deps = [
        "//3rdparty/jvm/com/fasterxml/jackson/core:jackson_core",
        "//3rdparty/jvm/com/fasterxml/jackson/core:jackson_databind",
        "//3rdparty/jvm/com/fasterxml/jackson/dataformat:jackson_dataformat_yaml",
        "//3rdparty/jvm/io/circe:circe_core",
        "//3rdparty/jvm/io/circe:circe_jackson25",
        "//3rdparty/jvm/org/typelevel:cats_core",
    ],
)

scala_library(
    name = "decoders",
    srcs = ["Decoders.scala"],
    visibility = ["//visibility:public"],
    deps = [
        ":depsmodel",
        "//3rdparty/jvm/io/circe:circe_core",
        "//3rdparty/jvm/io/circe:circe_generic",
    ],
)
