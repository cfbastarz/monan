/*
 * Copyright (C) 2019 Michael García Rodríguez
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package es.uvigo.esei.ephyslab.fortrananalyser;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.ResourceBundle;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * This class create a taskBar and support the logic part of the application. it
 * generate the quallity report
 *
 * @author Michael García Rodríguez
 * @version 1.9.8
 */
public final class TasksBarNoGUI {

    /**
     * the extension file to search.
     */
    private static final String EXTENSION = "f90";

    /**
     * the second extension file to search.
     */
    private static final String EXTENSION2 = "h90";

    /**
     * the third extension file to search.
     */
    private static final String EXTENSION3 = "f";

    /**
     * the path and the name of the file.
     */
    private static final String DEST = System.getProperty("user.home") + "/temp/QualityReport.pdf";

    /**
     * the path of the destination of the file
     */
    private static final String DESTPATH = System.getProperty("user.home") + "/temp";

    /**
     * corresponding position in the scores array of the score obtain on each
     * metric
     */
    private static final int[] POSITIONTABLESCORES = new int[]{5, 6, 7, 1, 2, 0, 3, 4, 8, 9, 10};

    /**
     * corresponding position in the finalScores array of the scores obtain on
     * each metric
     */
    private static final int[] POSITIONSFINALTABLESCORES = new int[]{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

    /**
     * Ends of the loops in Fortran
     */
    private static final String ENDDO = "END DO";

    /**
     * arrow to put in the report
     */
    private static final String ARROW = "\n\t--> ";

    /**
     * The main window of the GUI
     */
    private MainNoGUI mw;

    /**
     * the path of the directory to analyse.
     */
    private String path;

    /**
     * the assesment from file of the quality report.
     */
    private double assesment;

    /**
     * auxiliar variable to calcule the final calification.
     */
    private double auxNote;

    /**
     * the time when the process started.
     */
    private long timeStart;

    /**
     * the list with the parcial score of each file.
     */
    private ArrayList<Double> scores;

    /**
     * the list with all scores obtain by implicit none metric.
     */
    private ArrayList<Double> scoresImplicitNone;

    /**
     * the list with all scores obtain by percentage of comments metric.
     */
    private ArrayList<Double> scoresRatio;

    /**
     * the list with all scores obtain by nested loops metric.
     */
    private ArrayList<Double> scoresNestedLoops;

    /**
     * the list with all scores obtain by comments metric at the beginning.
     */
    private ArrayList<Double> scoresCommentsBeginning;

    /**
     * the list with all scores obtain by comments metric in variables.
     */
    private ArrayList<Double> scoresCommentsVariables;

    /**
     * the list with all scores obtain by comments metric in functions.
     */
    private ArrayList<Double> scoresCommentsfunction;

    /**
     * the list with all scores obtain by comments metric in subroutine.
     */
    private ArrayList<Double> scoresCommentsSubroutine;

    /**
     * the list with all scores obtain by control structures metric.
     */
    private ArrayList<Double> scoresCommentsControlStructures;

    /**
     * the list with scores obtain by exit metric.
     */
    private ArrayList<Double> scoresExit;

    /**
     * the list with scores obtain by cycle metric.
     */
    private ArrayList<Double> scoresCycle;

    /**
     * number of comentable elements in file
     */
    private double commentableElements;

    /**
     * number of comented elements in file
     */
    private double commentedElements;

    /**
     * total number of lines of the analysed software
     */
    private int totalNumLines;

    /**
     * Sum of all scores for each file analysed
     */
    private double partialCalification;

    /**
     * Names of each file
     */
    private ArrayList<String> fileNames;

    /**
     * Scores obtain by each file
     */
    private ArrayList<Double> fileScores;

    /**
     * The list of all files to analyse
     */
    private List<File> filesInFolder;

    /**
     * All score from cyclomatic complexity
     */
    private ArrayList<Double> cycloScores;

    /**
     * the string resources i18n.
     */
    ResourceBundle messages;

    /**
     * Constructor of the class with GUI
     *
     * @param mw the iframe
     * @param path the path of file
     * @param messages all strings for build the report
     */
    TasksBarNoGUI(MainNoGUI mw, String path, ResourceBundle messages) {

        initializeVariables();

        this.mw = mw;
        this.messages = messages;
        this.path = path;

    }

    /**
     * Constructor of the class without GUI
     *
     * @param path the path of the file
     * @param messages all string to build the report
     */
    TasksBarNoGUI(String path, ResourceBundle messages) {
        initializeVariables();

        this.messages = messages;
        this.path = path;

    }

    /**
     * Initialization of all variables declared in this class
     */
    public void initializeVariables() {
        this.scores = new ArrayList<>();
        this.scoresImplicitNone = new ArrayList<>();
        this.scoresRatio = new ArrayList<>();
        this.scoresNestedLoops = new ArrayList<>();
        this.scoresCommentsBeginning = new ArrayList<>();
        this.scoresCommentsVariables = new ArrayList<>();
        this.scoresCommentsfunction = new ArrayList<>();
        this.scoresCommentsSubroutine = new ArrayList<>();
        this.scoresCommentsControlStructures = new ArrayList<>();
        this.scoresExit = new ArrayList<>();
        this.scoresCycle = new ArrayList<>();
        this.fileNames = new ArrayList<>();
        this.fileScores = new ArrayList<>();
        this.filesInFolder = new ArrayList<>();
        this.commentableElements = 0.0;
        this.commentedElements = 0.0;
        this.partialCalification = 0.0;
        this.assesment = 0.0;
        this.cycloScores = new ArrayList<>();
    }

    /**
     * Empty constructor
     */
    public TasksBarNoGUI() {
        //throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    /**
     * In background, this method obtains all the files in an specific directory
     * and subdirectories and analyse them where the extension of the files are
     * ".f90" or ".h90" or ".f"
     *
     *
     * @return the note of the analysed software
     * @throws java.lang.Exception in case something wrong with intput/output
     * file
     */

    protected Void doInBackground() throws Exception {

        /**
         * initialice all arrayList and global variables
         */
        this.scoresImplicitNone.clear();
        this.scoresRatio.clear();
        this.scoresNestedLoops.clear();
        this.scoresCommentsBeginning.clear();
        this.scoresCommentsVariables.clear();
        this.scoresCommentsfunction.clear();
        this.scoresCommentsSubroutine.clear();
        this.scoresCommentsControlStructures.clear();
        this.scoresExit.clear();
        this.scoresCycle.clear();
        this.fileNames.clear();
        this.fileScores.clear();
        this.filesInFolder.clear();
        this.commentableElements = 0.0;
        this.commentedElements = 0.0;
        this.totalNumLines = 0;
        this.partialCalification = 0.0;
        this.cycloScores.clear();

        System.out.println("doInBackground");

        PDF pdf;
        int countNumberOfFiles = 0;
        auxNote = 0.0;

        double percentage = 0.0;

        try {

            String auxDir = "";
            pdf = new PDF();
            String extensionFile = "";

            //start the duration of the analysis
            timeStart = System.currentTimeMillis();

            percentage += 1.0;
            // publish((int) percentage);

            checkTempFileExist();

            pdf.createPdf(TasksBarNoGUI.DEST, this.messages.getLocale());

            scanFilesInDirectory(this.path, filesInFolder);

            /**
             * for each file of the directory and subdirectory
             */
            for (File file : filesInFolder) {

                this.scores.clear();
                extensionFile = getFileExtension(file).toLowerCase();

                /**
                 * Check if the file is not empty
                 */
                if (file.length() > 0) {
                    /**
                     * If it is a new directory, the path is added into the
                     * report.
                     */
                    if (!auxDir.equals(getPathFromFile(file))
                            && (extensionFile.equals(TasksBarNoGUI.EXTENSION)
                            || extensionFile.equals(TasksBarNoGUI.EXTENSION2)
                            || extensionFile.equals(TasksBarNoGUI.EXTENSION3))) {
                        auxDir = getPathFromFile(file);
                        pdf.addSection(auxDir);
                    }

                    /**
                     * If it is a new file of fortran code, the path is added
                     * into the report.
                     */
                    if (extensionFile.equals(TasksBarNoGUI.EXTENSION)
                            || extensionFile.equals(TasksBarNoGUI.EXTENSION2)
                            || extensionFile.equals(TasksBarNoGUI.EXTENSION3)) {
                        pdf.addSubSection(file.getName());
                        this.fileNames.add(file.getName());
                        pdf.addResult(analyseFile(file.getAbsolutePath()));
                        pdf.addTableScores(scores, this.messages, 13, 1, TasksBarNoGUI.POSITIONTABLESCORES);
                        countNumberOfFiles++;
                        this.fileScores.add(assesment);
                        pdf.addScoreResult(this.messages.getString("noteFile") + String.format("%.3f", assesment));
                    }

                    percentage += 98.0 / filesInFolder.size();
                    // publish((int) percentage);
                }
            }

            /**
             * the list scores is reused to stock the average of all metrics.
             */
            this.scores.clear();
            this.scores.add(calculateAverage(this.scoresImplicitNone));
            this.scores.add(calculateAverage(this.scoresRatio));
            this.scores.add(calculateAverage(this.scoresNestedLoops));
            this.scores.add(calculateAverage(this.scoresCommentsBeginning));
            this.scores.add(calculateAverage(this.scoresCommentsVariables));
            this.scores.add(calculateAverage(this.scoresCommentsfunction));
            this.scores.add(calculateAverage(this.scoresCommentsSubroutine));
            this.scores.add(calculateAverage(this.scoresCommentsControlStructures));
            this.scores.add(calculateAverage(this.scoresExit));
            this.scores.add(calculateAverage(this.scoresCycle));
            this.scores.add(calculateAverage(this.cycloScores));

            /**
             * Check if the software analysed have not Fortran files
             */
            if (!this.scores.get(0).isNaN()) {
                pdf.addSection(this.messages.getString("summary"));
                pdf.addFinalSummary(this.fileScores, this.fileNames, this.messages);
                pdf.addSummaryInformation(this.messages.getString("totalNumberOfFiles") + " " + countNumberOfFiles);
                pdf.addSummaryInformation(this.messages.getString("totalNumberOfLines") + " " + this.totalNumLines);
                pdf.addSubSectionInBold(this.messages.getString("finalTable"));
                pdf.addTableScores(this.scores, this.messages, 15, 0, TasksBarNoGUI.POSITIONSFINALTABLESCORES);

                /**
                 * Checking if there are not files analysed
                 */
                if (this.totalNumLines != 0) {
                    auxNote = partialCalification / this.totalNumLines;
                }

                pdf.addFinalNote(this.messages.getString("arithmeticAverage") + " " + String.format(Locale.ROOT, "%.3f", auxNote));
            }

            pdf.closePDF();
            partialCalification = 0.0;
            percentage = 100;
            this.totalNumLines = 0;
            // publish((int) percentage);

        } catch (IOException ex) {
            Logger.getLogger(MainNoGUI.class.getName()).log(Level.SEVERE, null, ex);
        }

        return null;
    }

    /**
     * List recursively files in a specific directory and each subdirectories.
     *
     * @param directoryName the root directory where the search start
     * @param files list of each file found
     */
    public static void scanFilesInDirectory(String directoryName, List<File> files) {

        File directory = new File(directoryName);
        File[] fList = directory.listFiles();
        if (fList != null) {
            for (File file : fList) {
                if (file.isFile()) {
                    files.add(file);
                } else if (file.isDirectory()) {
                    scanFilesInDirectory(file.getAbsolutePath(), files);
                }
            }
        }
    }

    /**
     * This method obtains the path from file without it name
     *
     * @param file the file that you want to get the path without it name
     * @return the path from a file
     */
    public static String getPathFromFile(File file) {

        return file.getAbsolutePath().
                substring(0, file.getAbsolutePath().lastIndexOf(File.separator));

    }

    /**
     * This method obtains the extension of a file
     *
     * @param file the file that we want to check the extension
     * @return the extension of the file
     */
    public static String getFileExtension(File file) {

        String name = file.getName();
        try {
            return name.substring(name.lastIndexOf('.') + 1);
        } catch (Exception e) {
            return "";
        }
    }

    /**
     * Check if the temp directory exists. If it not exists, this method create
     * it.
     */
    private static void checkTempFileExist() {

        if (!Paths.get(TasksBarNoGUI.DEST).toFile().exists()) {
            new File(TasksBarNoGUI.DESTPATH).mkdirs();
        }
    }

    /**
     * It call all the analyses from a file
     *
     * @param pathFile the path from the file to analyse
     * @return the result with all the output data
     * @throws IOException in case something wrong with intput/output file
     */
    public String analyseFile(String pathFile) throws IOException {

        String result = "";
        assesment = 0.0;
        double ratio = 0.0;
        double avgCyclo = 0.0;
        int numLines = analyseNumberOfLines(pathFile);
        boolean useImplicitNone = analyseUseImplicitNone(pathFile);
        boolean checkNestedLoops = analyseNestedLoops(pathFile);
        boolean useExit = analyseUseExit(pathFile);
        boolean useCycle = analyseUseCycle(pathFile);
        int numFunctions = analyseNumFunctions(pathFile);
        int numSubroutines = analyseNumberSubroutines(pathFile);
        int numVariables = analyseNumberOfDeclaredVariables(pathFile);
        String goodComments = analyseGoodComment(pathFile);
        CycloComplexity cc = new CycloComplexity();
        String cycloResult = cc.calculateComplexitySimpleCalcule(pathFile, this.messages);

        this.commentableElements += numFunctions;
        this.commentableElements += numSubroutines;
        this.commentableElements += numVariables;

        /**
         * count the number of lines in the file
         */
        result += this.messages.getString("numberOfLines") + numLines;
        result += "\n";

        this.totalNumLines += numLines;

        /**
         * 6. Use or not use the sentence IMPLICIT NONE
         */
        result += this.messages.getString("implicitNone") + useImplicitNone;
        result += "\n";

        /**
         * in case the sentente IMPLICIT NONE is used
         */
        if (useImplicitNone) {
            assesment += 2.0;
            this.scores.add(2.0);
            this.scoresImplicitNone.add(2.0);

        } else {
            this.scores.add(0.0);
            this.scoresImplicitNone.add(0.0);
        }

        /**
         * count the number of functions declared
         */
        result += this.messages.getString("numFunctions") + numFunctions;
        result += "\n";

        /**
         * count the number of subroutines calls
         */
        result += this.messages.getString("subroutinesCall") + analyseNumCalls(pathFile);
        result += "\n";

        /**
         * 7. calcule the ratio and show it in percentage in the report
         */
        if (commentableElements > 0.0) {
            ratio = (commentedElements / commentableElements);
        }

        result += this.messages.getString("ratio") + String.format(Locale.ROOT, "%.2f", (ratio * 100)) + "%";
        result += " \n";

        ratio = ratio * 2.0;

        assesment += ratio;
        this.scores.add(ratio);
        this.scoresRatio.add(ratio);

        /**
         * count the number of variables declared
         */
        result += this.messages.getString("numVariables") + numVariables;
        result += "\n";

        /**
         * 8. check the Nested loops
         */
        result += this.messages.getString("nestedLoops") + checkNestedLoops;
        result += "\n";

        /**
         * in case the nestedLoop are ok
         */
        if (checkNestedLoops) {
            assesment += 2.0;
            this.scores.add(2.0);
            this.scoresNestedLoops.add(2.0);
        } else {
            this.scores.add(0.0);
            this.scoresNestedLoops.add(0.0);
        }

        /**
         * Good comments in file
         */
        result += this.messages.getString("goodComments") + goodComments;
        result += "\n";

        /**
         * Check the number of declared subroutines
         */
        result += this.messages.getString("subroutines") + numSubroutines;
        result += "\n";

        /**
         * 9. Check the use of EXIT
         */
        result += this.messages.getString("exit") + useExit;
        result += "\n";

        /**
         * In case the sentece EXIT is used
         */
        if (useExit) {
            assesment += 1.0;
            this.scores.add(1.0);
            this.scoresExit.add(1.0);
        } else {
            this.scores.add(0.0);
            this.scoresExit.add(0.0);
        }

        /**
         * 10. Check the use of CYCLE
         */
        result += this.messages.getString("cycle") + useCycle;
        result += "\n";

        /**
         * in case the sentence CYCLE is used
         */
        if (useCycle) {
            assesment += 1.0;
            this.scores.add(1.0);
            this.scoresCycle.add(1.0);
        } else {
            this.scores.add(0.0);
            this.scoresCycle.add(0.0);
        }

        this.partialCalification += this.assesment * numLines;

        /**
         * 11. Add the Cyclomatic complexity.
         */
        if (!cycloResult.isEmpty()) {
            result += "\n";
            result += this.messages.getString("cyclomaticComplexity").toUpperCase();
            result += "\n\n";
            result += cycloResult;
            avgCyclo = calculateAverage(cc.getScoresCC());
            this.cycloScores.add(avgCyclo);
            this.scores.add(avgCyclo);
        }
        else{
            
            this.scores.add(0.0);
        }

        return result;

    }

    /**
     * This method obtains the number of lines of a file
     *
     * @param filePath the path of the file
     * @return the number of lines from file
     * @throws IOException in case something wrong with intput/output file
     */
    public static int analyseNumberOfLines(String filePath) throws IOException {

        int count = 0;
        String line = "";
        File file = new File(filePath);

        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {

            while ((line = b.readLine()) != null) {
                count++;
            }
        }

        return count;
    }

    /**
     * This method analyse if the sentence implicit none is used in each line
     * from a file
     *
     * @param filePath of the file analysed
     * @return boolean with the answer to the use of implicit none sentence
     * @throws IOException in case something wrong with intput/output file
     */
    public static boolean analyseUseImplicitNone(String filePath) throws IOException {

        String chain = "";
        File file = new File(filePath);

        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {
                chain = chain.toUpperCase();
                if (!chain.contains("!") && chain.contains("IMPLICIT NONE")) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * This method analyse the number of functions in file filePath
     *
     * @param filePath The path from file to analyse
     * @return the number of functions in this file
     * @throws IOException in case something wrong with intput/output file
     */
    public static int analyseNumFunctions(String filePath) throws IOException {

        int count = 0;
        String chain = "";
        File file = new File(filePath);

        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {
                chain = chain.toUpperCase();
                if (!chain.contains("!")
                        && !chain.contains("END FUNCTION ")
                        && chain.contains("FUNCTION ")) {
                    count++;
                }
            }
        }

        return count;
    }

    /**
     * This method analyse the number of subroutines are called in this file
     *
     * @param filePath the absolute path from file
     * @return the number of subroutines calls
     * @throws IOException in case something wrong with intput/output file
     */
    public static int analyseNumCalls(String filePath) throws IOException {
        int count = 0;
        String chain = "";
        File file = new File(filePath);

        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {
                chain = chain.toUpperCase();
                if (!chain.contains("!") && chain.contains("CALL")) {
                    count++;
                }
            }
        }

        return count;
    }

    /**
     * This method analyse the use of the CYCLE sentence in the file. It is used
     * in loops to avoid making a certain sentence, so that it continues to
     * iterate to the next element. With they use, the code is more efficient.
     *
     * @param filePath of the file analysed
     * @return boolean about the answer to the use of CYCLE sentence
     * @throws IOException in case something wrong with intput/output file
     */
    public static boolean analyseUseCycle(String filePath) throws IOException {

        String chain = "";
        File file = new File(filePath);
        int numCycles = 0;
        int numLoops = 0;

        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {

                chain = chain.toUpperCase();

                /**
                 * check if there are a loop.
                 */
                if (!chain.contains("!")
                        && !chain.contains(TasksBarNoGUI.ENDDO)
                        && chain.contains("DO")) {
                    numLoops++;
                }
                /**
                 * check if the chain is a declaration of a variable and it is
                 * not a comment.
                 */
                if (chain.contains("CYCLE")
                        && !chain.contains("!")) {

                    numCycles++;
                }
            }
        }

        return numLoops == numCycles;
    }

    /**
     * This method check if the EXIT sentence is used in the file. EXIT sentence
     * is used to go out of a loop, so the code is more efficient
     *
     * @param filePath of the file analysed
     * @return boolean of the answer to the use of EXIT sentence
     * @throws IOException in case something wrong with intput/output file
     */
    public static boolean analyseUseExit(String filePath) throws IOException {

        String chain = "";
        File file = new File(filePath);
        int numLoops = 0;
        int numExit = 0;
        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {

                chain = chain.toUpperCase();
                /**
                 * check if they are a loop.
                 */
                if (!chain.contains("!")
                        && !chain.contains(TasksBarNoGUI.ENDDO)
                        && chain.contains("DO")) {
                    numLoops++;
                }

                /**
                 * check if the chain is a declaration of a variable and it is
                 * not a comment.
                 */
                if ((chain.contains("EXIT"))
                        && !chain.contains("!")) {

                    numExit++;
                }

            }
        }
        return numLoops == numExit;
    }

    /**
     * This method analyse the number of subroutines declared in a file
     *
     * @param filePath of the file analysed
     * @return the number of subroutines
     * @throws IOException in case something wrong with intput/output file
     */
    public static int analyseNumberSubroutines(String filePath) throws IOException {

        String chain = "";
        int count = 0;
        File file = new File(filePath);

        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {
                chain = chain.toUpperCase();
                if (chain.contains("SUBROUTINE")
                        && !chain.contains("!")
                        && chain.contains("END SUBROUTINE")) {
                    count++;
                }
            }
        }

        return count;
    }

    /**
     * This method analyse the number of Nested loops there are. If this number
     * is greater than 3 or smaller than 0 AND this line don't have a comment ,
     * it is consider a bad programming practice.
     *
     * @param filePath of the file analysed
     * @return boolean with the use of nested loops
     * @throws IOException in case something wrong with intput/output file
     */
    public static boolean analyseNestedLoops(String filePath) throws IOException {

        String chain = "";
        int nestedLoops = 0;
        File file = new File(filePath);
        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {

                chain = chain.toUpperCase();

                if (!chain.contains("!")
                        && !chain.contains(TasksBarNoGUI.ENDDO)
                        && chain.contains("DO")) {

                    nestedLoops++;

                    if (nestedLoops > 3) {
                        return false;
                    }

                }

                if (!chain.contains("!")
                        && chain.contains(TasksBarNoGUI.ENDDO)) {
                    nestedLoops--;
                    if (nestedLoops < 0) {
                        return false;
                    }
                }

            }

        }

        /**
         * In case there are not nested loops in file, Else there are nested
         * loops within close sentence.
         */
        return nestedLoops == 0;

    }

    /**
     * This method count the number of declared variables in a file
     *
     * @param filePath of the file analysed
     * @return the number of declared variables
     * @throws IOException in case something wrong with intput/output file
     */
    public static int analyseNumberOfDeclaredVariables(String filePath) throws IOException {
        String chain = "";
        int count = 0;
        File file = new File(filePath);

        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {
                if (chain.contains("::")) {
                    count++;

                    if (chain.contains(",") && chain.indexOf("::") <= chain.indexOf(',')) {
                        count++;
                    }
                }
            }
        }

        return count;
    }

    /**
     * This method check if all comments are good. This is: 1.- the functions
     * are commented after or before the declaration. 2.- the variables are
     * commented after or before the declaration. 3.- the subrutines are
     * commented after or befor the declaration. 4.- the three first or more
     * lines of a file are commented.
     *
     * @param filePath of the file analysed
     * @return the paragraph to add to the pdf file
     * @throws IOException in case something wrong with intput/output file
     */
    public String analyseGoodComment(String filePath) throws IOException {

        String sb = "";
        boolean goodCommentFunctions = this.analyseGoodCommentFunctions(filePath);
        boolean goodCommentInitDoc = TasksBarNoGUI.analyseGoodCommentInitDoc(filePath);
        boolean goodCommentVariables = this.analyseGoodCommentedVariables(filePath);
        boolean goodCommentSubroutines = this.analyseGoodCommentSubroutines(filePath);
        boolean goodCommentControlStructures = TasksBarNoGUI.analyseGoodCommentControlStructures(filePath);

        /**
         * good comment in functions
         */
        sb = TasksBarNoGUI.ARROW + this.messages.getString("function") + goodCommentFunctions;

        /**
         * good comment at the begining of the document
         */
        sb += TasksBarNoGUI.ARROW + this.messages.getString("initDoc") + goodCommentInitDoc;

        /**
         * good comment at variables declaration
         */
        sb += TasksBarNoGUI.ARROW + this.messages.getString("variables") + goodCommentVariables;

        /**
         * good comment soubroutines declaration
         */
        sb += TasksBarNoGUI.ARROW + this.messages.getString("commentSubroutines") + goodCommentSubroutines;

        /**
         * good comment in control structures
         */
        sb += TasksBarNoGUI.ARROW + this.messages.getString("commentControlStructures") + goodCommentControlStructures;

        /**
         * 1. comments in functions
         */
        if (goodCommentFunctions) {
            assesment += 0.4;
            this.scores.add(0.4);
            this.scoresCommentsfunction.add(0.4);
        } else {
            this.scores.add(0.0);
            this.scoresCommentsfunction.add(0.0);
        }
        /**
         * 2. comments at the begining of the document
         */
        if (goodCommentInitDoc) {
            assesment += 0.4;
            this.scores.add(0.4);
            this.scoresCommentsBeginning.add(0.4);
        } else {
            this.scores.add(0.0);
            this.scoresCommentsBeginning.add(0.0);
        }
        /**
         * 3. comments in variables
         */
        if (goodCommentVariables) {
            assesment += 0.4;
            this.scores.add(0.4);
            this.scoresCommentsVariables.add(0.4);
        } else {
            this.scores.add(0.0);
            this.scoresCommentsVariables.add(0.0);
        }
        /**
         * 4. comments in subroutines
         */
        if (goodCommentSubroutines) {
            assesment += 0.4;
            this.scores.add(0.4);
            this.scoresCommentsSubroutine.add(0.4);
        } else {
            this.scores.add(0.0);
            this.scoresCommentsSubroutine.add(0.0);
        }
        /**
         * 5. comments in control structures
         */
        if (goodCommentControlStructures) {
            assesment += 0.4;
            this.scores.add(0.4);
            this.scoresCommentsControlStructures.add(0.4);
        } else {
            this.scores.add(0.0);
            this.scoresCommentsControlStructures.add(0.0);
        }

        return sb;

    }

    /**
     * This method analyse if the Control Structures are commented: ifs and
     * switch case
     *
     * @param filePath of the file analysed
     * @return boolean with the use of good comments in control structures
     * @throws IOException in case something wrong with intput/output file
     */
    public static boolean analyseGoodCommentControlStructures(String filePath) throws IOException {
        String chain = "";
        String previousChain = "";
        File file = new File(filePath);
        int numControlStructures = 0;
        int totalControlStructures = 0;

        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {

                chain = chain.toUpperCase();

                //check if it is a if structure declaration
                //or a select case structutre declaration
                if ((!chain.contains("!")
                        && !chain.contains("ENDIF")
                        && chain.contains("IF ("))
                        || (!chain.contains("!")
                        && !chain.contains("END SELECT")
                        && chain.contains("SELECT CASE"))) {
                    totalControlStructures++;

                    //check if the next line is a comment or the previous line
                    //is a comment
                    if (previousChain.contains("!")) {
                        numControlStructures++;
                    }
                }
                previousChain = chain;
            }
        }

        return totalControlStructures == numControlStructures;
    }

    /**
     * This method analyse if the declaration of subroutines are commented
     *
     * @param filePath of the file analysed
     * @return boolean to the use of good comments in subroutines
     * @throws IOException in case something wrong with intput/output file
     */
    public boolean analyseGoodCommentSubroutines(String filePath) throws IOException {

        String chain = "";
        String previousChain = "";
        File file = new File(filePath);
        int numSubroutines = 0;
        int totalSubroutines = 0;

        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {

                chain = chain.toUpperCase();

                //check if it is a declaration of a function
                if (!chain.contains("!")
                        && !chain.contains("END SUBROUTINE")
                        && chain.contains("SUBROUTINE")) {
                    totalSubroutines++;

                    //check if the next line is a comment or the previous line
                    //is a comment
                    if (previousChain.contains("!")) {
                        numSubroutines++;
                        this.commentedElements++;
                    }
                }
                previousChain = chain;
            }
        }
        return totalSubroutines == numSubroutines;
    }

    /**
     * This method analyse if for each variable, there is a comment to describe
     * what it done.
     *
     * @param filePath of the file analysed
     * @return boolean to the use of good comments in variables
     * @throws IOException in case something wrong with intput/output file
     */
    public boolean analyseGoodCommentedVariables(String filePath) throws IOException {

        String rowLine = "";
        String previousRowLine = "";
        File file = new File(filePath);
        int variablesCommented = 0;
        int totalVariables = 0;

        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((rowLine = b.readLine()) != null) {

                //check if the chain is a declaration of a variable and
                //it is not a comment 
                if (rowLine.contains("::")) {

                    totalVariables++;

                    //check if the next line is a comment or the previous line
                    //is a comment
                    if (previousRowLine.contains("!")) {
                        variablesCommented++;
                        commentedElements++;
                    }

                }
                previousRowLine = rowLine;
            }
        }
        return totalVariables == variablesCommented;
    }

    /**
     * This method analyse if there is a good comment at the beginning of a file
     *
     * @param filePath of the file analysed
     * @return boolean to the use of good comments at the beginning of the
     * document
     * @throws IOException in case something wrong with intput/output file
     */
    public static boolean analyseGoodCommentInitDoc(String filePath) throws IOException {

        String chain = "";
        int count = 0;
        int ite = 0;
        File file = new File(filePath);

        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null && count < 2 && ite < 3) {

                if (chain.contains("!")) {
                    count++;
                }

                ite++;
            }
        }

        return count > 1;
    }

    /**
     * This method check if the functions delcared in a file have or not have a
     * comment. The comment can be after or before the declaration of the
     * function. In addition, at the end of functions there are no comments.
     *
     * @param filePath of the file analysed
     * @return boolean of the use of good comments in functions
     * @throws IOException in case something wrong with intput/output file
     */
    public boolean analyseGoodCommentFunctions(String filePath) throws IOException {

        String chain = "";
        String previousChain = "";
        File file = new File(filePath);
        int numFunction = 0;
        int totalFunctions = 0;

        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {

                chain = chain.toUpperCase();
                //check if it is a declaration of a function
                if (!chain.contains("!")
                        && !chain.contains("END FUNCTION")
                        && chain.contains("FUNCTION")) {
                    totalFunctions++;

                    //check if the next line is a comment or the previous line
                    //is a comment
                    if (previousChain.contains("!")) {
                        numFunction++;
                        this.commentedElements++;
                    }
                }
                previousChain = chain;
            }
        }
        return totalFunctions == numFunction;
    }

    /**
     * This method transform a time in miliseconds in days, hours, minutes and
     * seconds
     *
     * @param millis the time to transform
     * @return the time in days, hours, minutes and seconds
     */
    public static String getDurationAnalyse(long millis) {

        if (millis < 0) {
            throw new IllegalArgumentException("Duration must be greater than zero!");
        }

        long days = TimeUnit.MILLISECONDS.toDays(millis);
        millis -= TimeUnit.DAYS.toMillis(days);
        long hours = TimeUnit.MILLISECONDS.toHours(millis);
        millis -= TimeUnit.HOURS.toMillis(hours);
        long minutes = TimeUnit.MILLISECONDS.toMinutes(millis);
        millis -= TimeUnit.MINUTES.toMillis(minutes);
        long seconds = TimeUnit.MILLISECONDS.toSeconds(millis);
        millis -= TimeUnit.MILLISECONDS.toMillis(seconds);

        StringBuilder sb = new StringBuilder(64);
        sb.append(days);
        sb.append(" D ");
        sb.append(hours);
        sb.append(" h ");
        sb.append(minutes);
        sb.append(" min ");
        sb.append(seconds);
        sb.append(" s ");
        sb.append(millis);
        sb.append(" ms");

        return sb.toString();
    }

    /**
     * this method calculate the average of the values in a list.
     *
     * @param l list with all values
     * @return the average from list l
     */
    public static Double calculateAverage(List<Double> l) {

        Double aux = 0.0;

        if (!l.isEmpty()) {
            for (int i = 0; i < l.size(); i++) {
                aux += l.get(i);
            }

            return aux / l.size();
        } else {
            return 0.0;
        }
    }

    public static String getDEST() {
        return DEST;
    }

    public static String getDESTPATH() {
        return DESTPATH;
    }

    public static int[] getPOSITIONTABLESCORES() {
        return POSITIONTABLESCORES;
    }

    public static int[] getPOSITIONSFINALTABLESCORES() {
        return POSITIONSFINALTABLESCORES;
    }

    public static String getARROW() {
        return ARROW;
    }

    public static String getEXTENSION() {
        return EXTENSION;
    }

    public static String getEXTENSION2() {
        return EXTENSION2;
    }

    public static String getEXTENSION3() {
        return EXTENSION3;
    }

}
