import kotlin.test.Test
import kotlin.test.assertEquals

class MainKtTest {

    @Test
    fun solve1() {
        assertEquals(35L, solve1("example.txt") )
        assertEquals(289863851, solve1("input.txt"))
    }
}