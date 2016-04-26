// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core.debug

import java.io.File
import java.util.concurrent.ConcurrentHashMap

import org.ensime.api.{EnsimeConfig, LineSourcePosition}
import org.slf4j.LoggerFactory

import scala.collection.mutable
import org.ensime.config._
import org.ensime.util.file.RichFile
import org.scaladebugger.api.profiles.traits.info.LocationInfoProfile

/**
 * Represents a utility to map local source files provided by Ensime to
 * JDI locations.
 *
 * @param config The Ensime configuration used to load source files
 */
class SourceMap(private val config: EnsimeConfig) {
  /** Logger used within source map class. */
  private val logger = LoggerFactory.getLogger(this.getClass)

  /** Represents internal storage of local source files. */
  private lazy val sources: Set[File] = config.scalaSourceFiles.map(_.canon)
  private lazy val sourceMap: Map[String, Set[File]] = sources.groupBy(_.getName)

  // TODO: Provide cleaner generation of path map by finding root directory
  //       (or multiple directories) and filling in the map in advance
  import scala.collection.JavaConverters._
  private lazy val pathMap: mutable.Map[String, File] =
    new ConcurrentHashMap[String, File]().asScala

  /**
   * Creates a new LineSourcePosition instance from the given location.
   *
   * @param location The location to use when constructing the new
   *                 LineSourcePosition
   * @return Some LineSourcePosition if matching source file is found,
   *         otherwise None
   */
  def newLineSourcePosition(
    location: LocationInfoProfile
  ): Option[LineSourcePosition] = {
    findFileByLocation(location).map(f =>
      LineSourcePosition(f, location.lineNumber)
    )
  }

  /**
   * Finds the local source file mapping to the given location.
   *
   * @param location The location whose source file to find
   * @return Some file representing the local source, otherwise None
   */
  def findFileByLocation(location: LocationInfoProfile): Option[File] = {
    val path = location.trySourcePath.toOption

    path.flatMap(sourceForFilePath)
  }

  /**
   * Retrieves all current Scala sources available through Ensime with the
   * given file name.
   *
   * @param fileName The name of the file whose matches to retrieve
   * @return The set of sources whose file name match the given name
   */
  def sourcesForFileName(fileName: String): Set[File] =
    sourceMap.getOrElse(fileName, Set())

  /**
   * Retrieves the current Scala source available through Ensime with the
   * given file path.
   *
   * @param filePath The path of the file whose match to retrieve
   * @return Some source whose file path matches the given path, otherwise None
   */
  def sourceForFilePath(filePath: String): Option[File] = {
    // Check if we have a match in the cached path map first
    val cachedResult = pathMap.get(filePath)

    // If no cached result, search through all of sources to find a match
    val result = sources.find(_.getAbsolutePath.endsWith(filePath))

    // Store the found result as our new cached result
    if (cachedResult.isEmpty && result.nonEmpty)
      pathMap.put(filePath, result.get)

    cachedResult.orElse(result)
  }

  /**
   * Retrieves current Scala sources available through Ensime.
   *
   * @return The set of Scala source files
   */
  def canonicalSources: Set[File] = sources
}
