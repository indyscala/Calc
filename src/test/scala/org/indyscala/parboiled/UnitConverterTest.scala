package org.indyscala.parboiled

import org.parboiled2.ParseError
import org.specs2.mutable.{Before, Specification}

/**
 * Created by petarvlahu on 02.9.14.
 */
class UnitConverterTest extends Specification {
  "The 'UnitCoverter'" should {
    "add" in {
      UnitConverter.exec("1+2") === 3
    }
    "add 1+2!=4" in {
      UnitConverter.exec("1+2") !== 4
    }
    "subtract" in {
      UnitConverter.exec("5-2") === 3
    }
    "subtract 5-2!=5" in {
      UnitConverter.exec("5-2") !== 5
    }
    "multiply" in {
      UnitConverter.exec("2*2") === 4
    }
    "multiply 2*2!=3" in {
      UnitConverter.exec("2*2") !== 3
    }
    "divide" in {
      UnitConverter.exec("6/2") === 3
    }
    "divide 6/2!=2" in {
      UnitConverter.exec("6/2") !== 2
    }
    "respect operator precedence multiply first" in {
      UnitConverter.exec("2*2+4") === 8
    }
    "respect operator precedence multiply first 2*2+4!=12" in {
      UnitConverter.exec("2*2+4") !== 12
    }
    "respect operator precedence multiply last" in {
      UnitConverter.exec("2+2*4") === 10
    }
    "respect operator precedence multiply last 2+2*4!=16" in {
      UnitConverter.exec("2+2*4") !== 16
    }
    "handle brackets" in {
      UnitConverter.exec("(1+2)") === 3
    }
    "handle nested brackets" in {
      UnitConverter.exec("(((1+2)))") === 3
    }
    "override precedence with brackets first" in {
      UnitConverter.exec("(1+2)*2") === 6
    }
    "override precedence with brackets last" in {
      UnitConverter.exec("18/(1+2)") === 6
    }
    "(2*(4/2*(1+2)-4)+3-1-1)-1 = 4" in {
      UnitConverter.exec("(2*(4/2*(1+2)-4)+3-1-1)-1") === 4
    }

    "((( is an error" in {
      UnitConverter.exec("(((") should throwA[ParseError]
    }

  }
}
