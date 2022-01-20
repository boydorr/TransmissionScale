import com.sun.org.apache.xpath.internal.operations.Bool;

import java.io.FileWriter;
import java.io.IOException;

/**
 * Created by rebeccamancy on 11/09/2015.
 */
public class CSVWriter {
    // Delimiter used in CSV file
    private static final String COMMA_DELIMITER = ",";
    private static final String NEW_LINE_SEPARATOR = "\n";
    FileWriter fileWriter = null;

    public CSVWriter(String fileName) {
        try {
            fileWriter = new FileWriter(fileName);

        } catch (Exception e) {
            System.out.println("Error in CsvFileWriter !!! " + fileName);
            e.printStackTrace();
        }
    }

    public void appendLine(String s) {
        try {
            fileWriter.append(s);
            fileWriter.append(NEW_LINE_SEPARATOR);

        } catch (Exception e) {
            System.out.println("Error in EpidemicOutcomesWriter > CsvFileWriter !!!");
            e.printStackTrace();

        }
    }

    public void close() {
        try {
            fileWriter.flush();
            fileWriter.close();
        } catch (IOException e) {
            System.out.println("Error while flushing/closing fileWriter !!!");
            e.printStackTrace();
        }
    }
}
