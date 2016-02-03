package pictbliz.extractor

import scala.collection.mutable

abstract class DBSection {

  val indices: mutable.LongMap[Int]

}

// define specific aliases name -> index
class Heroes extends DBSection
class Skills extends DBSection
class Items extends DBSection
class Monsters extends DBSection
class MonsterGroups extends DBSection
class Terrains extends DBSection
class Attributes extends DBSection
class Conditions extends DBSection
class Animations extends DBSection
class TileSets extends DBSection
class Vocabulary extends DBSection
class SystemSettings extends DBSection
class Switches extends DBSection
class Variables extends DBSection
class CommonEvents extends DBSection

