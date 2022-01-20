import java.io.FileWriter;
import java.io.IOException;
import java.util.TreeSet;

/**
 * Created by rebeccamancy on 27/11/2015.
 * Used to write out average population vaccination coverage
 */
public class MixedPopCoverageWriter {

    // New line used in CSV file
    private static final String NEW_LINE_SEPARATOR = "\n";

    // CSV file header
    private static final String FILE_HEADER = PopCoverage.toStringCsvHeader();

    public static void writeCsvFile(String fileName, TreeSet<PopCoverage> popCoverageTreeSet) {
        /// System.out.println("In MixedPopCoverageWriter " + fileName + " " + popCoverageTreeSet.size());
        if (fileName.length()>0) {
            FileWriter fileWriter = null;

            try {
                fileWriter = new FileWriter(fileName);

                // Write the CSV file header and new line
                fileWriter.append(FILE_HEADER.toString());
                fileWriter.append(NEW_LINE_SEPARATOR);

                // Write a new coverage object to the CSV file
                for (PopCoverage c : popCoverageTreeSet) {
                    fileWriter.append(c.toStringCsv());
                    fileWriter.append(NEW_LINE_SEPARATOR);
                }

            } catch (Exception e) {
                System.out.println("Error in MixedPopCoverageWriter.writeCsvFile !!!");
                e.printStackTrace();
                System.exit(0);

            } finally {
                try {
                    fileWriter.flush();
                    fileWriter.close();
                } catch (IOException e) {
                    System.out.println("Error while flushing/closing fileWriter to write population coverage !!!");
                    e.printStackTrace();
                }
            }
        }
    }
}
