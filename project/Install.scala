import java.io.IOException
import java.nio.file._
import sbt._

object InstallCommand {
  // The logger used for the program.
  private lazy val logger = ConsoleLogger()

  // Config values.
  val manifestPath = "install_manifest.txt"
  val deployPath = "/usr/local/bin/clasp"
  val bash = """#!/usr/bin/env bash
CLASPPATH={cwd} java -jar {cwd}/target/clasp.jar"""

  // Performing an installation of the language into a given location. Creates
  // an installation.txt file in the main project directory to track
  // installation metadata for a complete uninstallation.
  def install = Command.command("install") { state =>
    val ns = Command.process("assembly", if (Files.exists(Paths.get(manifestPath)))
      Command.process("uninstall", state)
    else state)

    Files.write(Paths.get(deployPath), bash.replace("{cwd}", System.getProperty("user.dir")).getBytes)
    s"chmod u+x $deployPath".!
    Files.write(Paths.get(manifestPath), deployPath.getBytes)

    ns
  }

  // Using an installation.txt in the directory to remove installed files.
  def uninstall = Command.command("uninstall") { state =>
    if (!Files.exists(Paths.get(manifestPath))) {
      logger.error(s"Cannot uninstall without an $manifestPath")
      state
    } else {
      scala.io.Source.fromFile(manifestPath).getLines foreach { line =>
        try {
          Files.delete(Paths.get(line))
          logger.success(s"Removed '$line'.")
        } catch {
          case e: IOException =>
            logger.warn(s"Could not remove '$line'.")
        }
      }

      try {
        Files.delete(Paths.get(manifestPath))
      } catch {
        case e: IOException =>
          logger.error(s"Could not delete $manifestPath")
      }

      state
    }
  }
}
