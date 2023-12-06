import kotlin.test.Test
import kotlin.test.assertEquals

class MainKtTest {

    @Test
    fun solve1() {
        assertEquals(35L, solve1("example.txt") )
        assertEquals(289863851, solve1("input.txt"))
    }
    @Test
    fun solve2() {
        assertEquals(46L, solve2("example.txt"))
        assertEquals( 289863851, solve2("input.txt"))
    }
}