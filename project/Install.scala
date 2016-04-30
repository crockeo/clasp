import java.nio.file._
import sbt._

object InstallCommand {
  // Performing an installation of the language into a given location. Creates
  // an installation.txt file in the main project directory to track
  // installation metadata for a complete uninstallation.
  def install = Command.command("install") { state =>
    println("Testing")
    state
  }

  // Using an installation.txt in the directory to remove installed files.
  def uninstall = Command.command("uninstall") { state =>
    if (!Files.exists(Paths.get("installation.txt"))) {
    } else {

    }

    state
  }
}
