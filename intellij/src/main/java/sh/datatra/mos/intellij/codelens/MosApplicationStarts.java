package sh.datatra.mos.intellij.codelens;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

final class MosApplicationStarts {
    private static final Pattern BASIC_START =
            Pattern.compile("\\bbasic_start\\s*\\(\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*\\)");
    private static final Pattern PROGRAM_COUNTER = Pattern.compile("(?m)^(\\s*)(\\*)\\s*=");

    private MosApplicationStarts() {
    }

    static List<Start> find(CharSequence source) {
        String searchable = maskCommentsAndStrings(source);
        Map<Integer, Start> starts = new LinkedHashMap<>();

        Matcher basicStarts = BASIC_START.matcher(searchable);
        while (basicStarts.find()) {
            String label = basicStarts.group(1);
            Pattern labelPattern = Pattern.compile(
                    "(?m)^\\s*(" + Pattern.quote(label) + ")\\s*:"
            );
            Matcher labelMatch = labelPattern.matcher(searchable);
            if (labelMatch.find()) {
                starts.put(labelMatch.start(1), new Start(labelMatch.start(1), label.length()));
            }
        }

        Matcher programCounters = PROGRAM_COUNTER.matcher(searchable);
        while (programCounters.find()) {
            int offset = programCounters.start(2);
            starts.put(offset, new Start(offset, 1));
        }

        List<Start> result = new ArrayList<>(starts.values());
        result.sort(Comparator.comparingInt(Start::offset));
        return List.copyOf(result);
    }

    static String parseBuildEntry(String toml) {
        String section = "";
        for (String line : toml.split("\\R")) {
            if (section.isEmpty()) {
                Matcher dotted = Pattern.compile(
                        "^\\s*build\\s*\\.\\s*entry\\s*=\\s*[\"']([^\"']+)[\"']"
                ).matcher(line);
                if (dotted.find()) {
                    return dotted.group(1);
                }
            }
            Matcher sectionMatch = Pattern.compile(
                    "^\\s*\\[([^]]+)]\\s*(?:#.*)?$"
            ).matcher(line);
            if (sectionMatch.find()) {
                section = sectionMatch.group(1).trim();
                continue;
            }
            if (!section.equals("build")) {
                continue;
            }
            Matcher entry = Pattern.compile(
                    "^\\s*entry\\s*=\\s*[\"']([^\"']+)[\"']"
            ).matcher(line);
            if (entry.find()) {
                return entry.group(1);
            }
        }
        return "main.asm";
    }

    private static String maskCommentsAndStrings(CharSequence source) {
        char[] chars = source.toString().toCharArray();
        State state = State.CODE;
        int blockDepth = 0;
        boolean escaped = false;
        for (int index = 0; index < chars.length; index++) {
            char current = chars[index];
            char next = index + 1 < chars.length ? chars[index + 1] : '\0';
            if (state == State.LINE_COMMENT) {
                if (current == '\n') {
                    state = State.CODE;
                } else {
                    chars[index] = ' ';
                }
                continue;
            }
            if (state == State.BLOCK_COMMENT) {
                chars[index] = current == '\n' ? '\n' : ' ';
                if (current == '/' && next == '*') {
                    chars[++index] = ' ';
                    blockDepth++;
                } else if (current == '*' && next == '/') {
                    chars[++index] = ' ';
                    blockDepth--;
                    if (blockDepth == 0) {
                        state = State.CODE;
                    }
                }
                continue;
            }
            if (state == State.STRING) {
                chars[index] = current == '\n' ? '\n' : ' ';
                if (!escaped && current == '"') {
                    state = State.CODE;
                }
                escaped = !escaped && current == '\\';
                continue;
            }
            if (current == '/' && next == '/') {
                chars[index] = ' ';
                chars[++index] = ' ';
                state = State.LINE_COMMENT;
            } else if (current == '/' && next == '*') {
                chars[index] = ' ';
                chars[++index] = ' ';
                blockDepth = 1;
                state = State.BLOCK_COMMENT;
            } else if (current == '"') {
                chars[index] = ' ';
                escaped = false;
                state = State.STRING;
            }
        }
        return new String(chars);
    }

    record Start(int offset, int length) {
    }

    private enum State {
        CODE,
        LINE_COMMENT,
        BLOCK_COMMENT,
        STRING
    }
}
