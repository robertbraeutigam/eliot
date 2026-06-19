import org.jetbrains.kotlin.gradle.dsl.JvmTarget

// Eliot IntelliJ plugin: bundles the TextMate grammar (ide/textmate) for highlighting and launches the
// Eliot language server (ide/lsp) through Red Hat's LSP4IJ for diagnostics. See ide/intellij/README.md.
plugins {
    kotlin("jvm") version "2.1.0"
    id("org.jetbrains.intellij.platform") version "2.16.0"
}

group = providers.gradleProperty("pluginGroup").get()
version = providers.gradleProperty("pluginVersion").get()

repositories {
    mavenCentral()
    intellijPlatform {
        defaultRepositories()
    }
}

dependencies {
    intellijPlatform {
        create(
            providers.gradleProperty("platformType").get(),
            providers.gradleProperty("platformVersion").get(),
        )

        // TextMate Bundles ships with every IDE — bundled, not from the Marketplace. It owns the
        // com.intellij.textmate.bundleProvider extension point we register the grammar through.
        bundledPlugin("org.jetbrains.plugins.textmate")

        // LSP4IJ is a Marketplace plugin; declaring it here makes it available to runIde and the
        // <depends> in plugin.xml prompts users to install it. We do NOT bundle its lsp4j — the
        // server runs as a separate process with its own copy (see EliotConnectionProvider).
        plugin("com.redhat.devtools.lsp4ij:${providers.gradleProperty("lsp4ijVersion").get()}")

        pluginVerifier()
        zipSigner()
    }
}

intellijPlatform {
    pluginConfiguration {
        ideaVersion {
            sinceBuild = providers.gradleProperty("pluginSinceBuild")
            untilBuild = provider { null } // keep loading on newer IDEs
        }
    }
}

kotlin {
    compilerOptions {
        jvmTarget = JvmTarget.JVM_21
    }
}

java {
    sourceCompatibility = JavaVersion.VERSION_21
    targetCompatibility = JavaVersion.VERSION_21
}

// ---------------------------------------------------------------------------------------------------
// Bundling the language server.
//
// The server MUST travel as the per-module + dependency jars produced by ide/lsp/package.sh, never a
// fat assembly jar: Eliot's platform layers keep multiple files at the same resource path (e.g.
// eliot/lang/String.els in both the lang and stdlib layers) and a fat jar would collapse them, silently
// dropping a layer. package.sh is the single source of truth for that jar set. We copy its output into
// the plugin distribution under "<plugin>/server/lib/" and launch a child JVM with `-cp .../server/lib/*`
// (EliotConnectionProvider), which preserves the layered-resource semantics.
//
// The TextMate grammar (ide/textmate, a VS Code-extension-layout bundle) is copied to "<plugin>/textmate/"
// and registered by EliotTextMateBundleProvider, which needs a real on-disk path.
// ---------------------------------------------------------------------------------------------------

val packageServer by tasks.registering(Exec::class) {
    description = "Build the Eliot language-server jars (per-module + deps) via ide/lsp/package.sh."
    group = "build"
    workingDir = projectDir
    commandLine("bash", file("../lsp/package.sh").absolutePath)
    // package.sh recompiles from these sources; treat them as inputs so re-runs are needed after edits.
    inputs.dir(file("../lsp/src"))
    inputs.dir(file("../../lang/src"))
    inputs.dir(file("../../stdlib/src"))
    inputs.dir(file("../../eliotc/src"))
    inputs.dir(file("../../jvm/src")) // the JVM backend is bundled in compiler-lib for the "Run main" feature
    outputs.dir(file("../lsp/dist/lib"))
    outputs.dir(file("../lsp/dist/compiler-lib"))
}

tasks.withType<org.jetbrains.intellij.platform.gradle.tasks.PrepareSandboxTask> {
    dependsOn(packageServer)
    // "eliot" must match rootProject.name (the sandbox/installation plugin directory name).
    from(file("../lsp/dist/lib")) { into("eliot/server/lib") }
    // The JVM backend + ASM, used by the "Run main" run configuration to build an executable jar (the
    // resident LSP server does NOT load these — see EliotConnectionProvider vs. the run config's command).
    from(file("../lsp/dist/compiler-lib")) { into("eliot/compiler/lib") }
    from(file("../textmate")) {
        into("eliot/textmate")
        exclude("README.md")
    }
}
