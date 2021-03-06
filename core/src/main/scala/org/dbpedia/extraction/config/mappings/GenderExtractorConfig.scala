package org.dbpedia.extraction.config.mappings

object GenderExtractorConfig
{

    val pronounsMap = Map(
        "en" -> Map("she" -> "female", "her" -> "female", "he" -> "male", "his" -> "male", "him" -> "male", "herself" -> "female", "himself" -> "male",
                    "She" -> "female", "Her" -> "female", "He" -> "male", "His" -> "male", "Him" -> "male", "Herself" -> "female", "Himself" -> "male" //TODO why not just do case insensitive matches?
        )
    )

    val supportedLanguages = pronounsMap.keySet

    val minCount = 3       // absolute count of gender indicators

    val minDifference = 2  // has to by X many winner genders over second placed genders

}
