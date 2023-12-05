import java.io.File
import kotlin.math.min

// Pair of start and destination
fun getDestination(value: Long, mapping: java.util.ArrayList<Pair<LongRange, LongRange>>): Long {
    for (source in mapping) {
        if (value in source.first) {
            return source.second.first + value - source.first.first
        }
    }
    return value
}
val sections = 7 // 7 sections

fun parse(input: String): Pair<List<Long>, HashMap<Int, ArrayList<Pair<LongRange,LongRange>>>> {
    val content = File(input).readLines()
    val seeds = content[0].split(": ")[1].split(" ").map(String::toLong)
    var index = 2;
    val mappings : HashMap<Int, ArrayList<Pair<LongRange,LongRange>>> = HashMap()
    for (section in 0..sections) {
        index +=1; // skip the title, don't care.
        val mapping : ArrayList<Pair<LongRange,LongRange>> = ArrayList();
        while(index < content.size && content[index] != "") {
            val parts = content[index].split(" ")
            val sourceInt = parts[1].toLong();
            val destInt = parts[0].toLong();
            val offset = parts[2].toLong();
            val source : LongRange = sourceInt..(sourceInt+offset)
            val dest : LongRange = destInt..(destInt+offset)
            mapping.add(Pair(source, dest))
            index += 1
        }
        index+=1; // skip empty line
        mappings[section] = mapping
    }

    return Pair(seeds, mappings)
}


fun solve1(input: String): Long {
    val parsed = parse(input);
    val (first, second) = parsed
    var minimum = Long.MAX_VALUE;
    for(seed in first) {
        var section = 0
        var dest = seed;
        while (section < sections){
            dest = getDestination(dest, second[section]!!);
            section += 1;
        }
        minimum = min(minimum, dest)
    }
    return minimum
}
