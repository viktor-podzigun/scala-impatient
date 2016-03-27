import java.io.IOException;

public class Chapter15Task5 {

    public static String scalaFileToString(final String file) {
        try {
            return Chapter15.fileToString(file);
        } catch (final IOException e) {
            throw new RuntimeException(e);
        }
    }
}
