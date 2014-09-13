package org.indyscala.parboiled

import org.parboiled2.ParseError
import org.specs2.mutable.{Before, Specification}

/**
 * Created by petarvlahu on 02.9.14.
 */
class CalcTest extends Specification {
  "The 'Calculator'" should {
    "add" in {
      Calc.exec("1+2") === 3
    }
    "add 1+2!=4" in {
      Calc.exec("1+2") !== 4
    }
    "subtract" in {
      Calc.exec("5-2") === 3
    }
    "subtract 5-2!=5" in {
      Calc.exec("5-2") !== 5
    }
    "multiply" in {
      Calc.exec("2*2") === 4
    }
    "multiply 2*2!=3" in {
      Calc.exec("2*2") !== 3
    }
    "divide" in {
      Calc.exec("6/2") === 3
    }
    "divide 6/2!=2" in {
      Calc.exec("6/2") !== 2
    }
    "respect operator precedence multiply first" in {
      Calc.exec("2*2+4") === 8
    }
    "respect operator precedence multiply first 2*2+4!=12" in {
      Calc.exec("2*2+4") !== 12
    }
    "respect operator precedence multiply last" in {
      Calc.exec("2+2*4") === 10
    }
    "respect operator precedence multiply last 2+2*4!=16" in {
      Calc.exec("2+2*4") !== 16
    }
    "handle brackets" in {
      Calc.exec("(1+2)") === 3
    }
    "handle nested brackets" in {
      Calc.exec("(((1+2)))") === 3
    }
    "override precedence with brackets first" in {
      Calc.exec("(1+2)*2") === 6
    }
    "override precedence with brackets last" in {
      Calc.exec("18/(1+2)") === 6
    }
    "(2*(4/2*(1+2)-4)+3-1-1)-1 = 4" in {
      Calc.exec("(2*(4/2*(1+2)-4)+3-1-1)-1") === 4
    }

    "((( is an error" in {
      Calc.exec("(((") should throwA[ParseError]
    }

    "2^2=4" in {
      Calc.exec("2^2") === 4
    }

    "2^2^2=16" in {
      Calc.exec("2^2^2") === 16
    }

    "2^2^(1*1+1)=16" in {
      Calc.exec("2^2^(1*1+1)") === 16
    }

    "sqrt(4)=2" in {
      Calc.exec("sqrt(4)") === 2
    }

    "sqrt(8+2*4+9)=5" in {
      Calc.exec("sqrt(8+2*4+9)") === 5
    }

    "sqrt((8+2)*4+9)=7" in {
      Calc.exec("sqrt((8+2)*4+9)") === 7
    }

    "1.0+2.2=3.2" in {
      Calc.exec("1.0+2.2") === 3.2
    }

    "1+2.2=3.2" in {
      Calc.exec("1+2.2") === 3.2
    }

    "1,0+2,2=3.2" in {
      Calc.exec("1,0+2,2") === 3.2
    }

    "1.0+2,2=3.2" in {
      Calc.exec("1.0+2,2") === 3.2
    }

    "1+2,2=3.2" in {
      Calc.exec("1+2,2") === 3.2
    }
  }
}
