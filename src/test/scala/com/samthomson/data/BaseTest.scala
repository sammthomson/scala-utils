package com.samthomson.data

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, FlatSpec}

trait BaseTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks
