// The Eliot IntelliJ plugin is a self-contained Gradle build (the IntelliJ Platform Gradle Plugin has
// no Mill equivalent). It lives under ide/ alongside the other editor tooling but is NOT part of the
// top-level Mill build; it *calls* Mill (via ide/lsp/package.sh) to obtain the language-server jars.
//
// rootProject.name doubles as the plugin directory name inside the IDE sandbox / installation, so the
// bundled server jars and TextMate grammar are copied under "<rootProject.name>/..." by prepareSandbox
// (see build.gradle.kts). At runtime the plugin resolves those via its own pluginPath, not this name.
rootProject.name = "eliot"

pluginManagement {
    repositories {
        gradlePluginPortal()
        mavenCentral()
    }
}
