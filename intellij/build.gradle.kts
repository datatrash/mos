plugins {
    java
    id("org.jetbrains.intellij.platform") version "2.18.1"
}

group = "sh.datatra.mos"
version = "0.1.2"

repositories {
    mavenCentral()
    intellijPlatform {
        defaultRepositories()
    }
}

dependencies {
    implementation("org.apache.commons:commons-compress:1.28.0")

    intellijPlatform {
        intellijIdeaCommunity("2024.2")
        bundledPlugin("org.jetbrains.plugins.textmate")
        bundledPlugin("com.intellij.modules.json")
        plugin("com.redhat.devtools.lsp4ij:0.21.0")
        testFramework(org.jetbrains.intellij.platform.gradle.TestFrameworkType.Platform)
        pluginVerifier()
        zipSigner()
    }

    testImplementation("org.junit.jupiter:junit-jupiter:5.13.4")
    testImplementation("junit:junit:4.13.2")
    testRuntimeOnly("org.junit.platform:junit-platform-launcher:1.13.4")
    testRuntimeOnly("org.junit.vintage:junit-vintage-engine:5.13.4")
}

java {
    // Target the JVM bytecode level of the IntelliJ platform (17). The compiler JVM runs on the
    // same JDK version the underlying IDE ships with (21), so the plugin and its test sandbox stay
    // byte-for-byte compatible with the bundled JetBrains Runtime.
    toolchain {
        languageVersion = JavaLanguageVersion.of(21)
    }
}

tasks {
    withType<JavaCompile>().configureEach {
        options.release = 17
    }
    test {
        useJUnitPlatform()
    }
}

intellijPlatform {
    pluginConfiguration {
        name = "MOS"
        version = project.version.toString()
        ideaVersion {
            sinceBuild = "242"
        }
        vendor {
            name = "datatrash"
            url = "https://mos.datatra.sh/"
        }
        description = """
            Language, build, test, and debugging support for the MOS assembler.
        """.trimIndent()
    }

    // Signing material is supplied either inline (CI, straight from secrets) or as a path to a
    // file (handier locally). Only one of each pair should be set; an unset environment variable
    // leaves the corresponding property absent.
    signing {
        certificateChain = providers.environmentVariable("CERTIFICATE_CHAIN")
        certificateChainFile =
            layout.projectDirectory.file(providers.environmentVariable("CERTIFICATE_CHAIN_FILE"))
        privateKey = providers.environmentVariable("PRIVATE_KEY")
        privateKeyFile =
            layout.projectDirectory.file(providers.environmentVariable("PRIVATE_KEY_FILE"))
        password = providers.environmentVariable("PRIVATE_KEY_PASSWORD")
    }

    publishing {
        token = providers.environmentVariable("PUBLISH_TOKEN")
    }
}
