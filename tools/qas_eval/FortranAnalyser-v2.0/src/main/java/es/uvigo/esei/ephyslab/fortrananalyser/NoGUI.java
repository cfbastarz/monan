/*
 * Copyright (C) 2019 Michael Garvcía Rodríguez
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
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.annotation.Resource;

/**
 * This class create a NoGUI and support the logic part of the application
 * without graphical intercface, after that it generate the quallity report.
 *
 * @author Michael García Rodríguez
 * @version 1.9.8
 */
public class NoGUI {
    
    /**
     * the path of the destination of the file
     */
    public static final String DEST_PATH = System.getProperty("user.home") + "/temp";
    
    /**
     * the path of the directory to analyse.
     */
    private String dirPath;

    /**
     * the assesment from file of the quality report.
     */
    private double assesment;

    /**
     * the list with the parcial score of each file.
     */
    private ArrayList<Double> scoresNoGUI;

    /**
     * the list with all scores obtain by implicit none metric.
     */
    private ArrayList<Double> scoresImplicitNoneNoGUI;

    /**
     * the list with all scores obtain by percentage of comments metric.
     */
    private ArrayList<Double> scoresRatioNoGUI;

    /**
     * the list with all scores obtain by nested loops metric.
     */
    private ArrayList<Double> scoresNestedLoopsNoGUI;

    /**
     * the list with all scores obtain by comments metric at the beginning.
     */
    private ArrayList<Double> scoresCommentsBeginningNoGUI;

    /**
     * the list with all scores obtain by comments metric in variables.
     */
    private ArrayList<Double> scoresCommentsVariablesNoGUI;

    /**
     * the list with all scores obtain by comments metric in functions.
     */
    private ArrayList<Double> scoresCommentsfunctionNoGUI;

    /**
     * the list with all scores obtain by comments metric in subroutine.
     */
    private ArrayList<Double> scoresCommentsSubroutineNoGUI;

    /**
     * the list with all scores obtain by control structures metric.
     */
    private ArrayList<Double> scoresCommentsControlStructuresNoGUI;

    /**
     * the list with scores obtain by exit metric.
     */
    private ArrayList<Double> scoresExitNoGUI;

    /**
     * the list with scores obtain by cycle metric.
     */
    private ArrayList<Double> scoresCycleNoGUI;

    /**
     * number of comentable elements in file
     */
    private double commentableElementsNoGUI;

    /**
     * number of comented elements in file
     */
    private double commentedElementsNoGUI;

    /**
     * total number of lines of the analysed software
     */
    private int totalNumLinesNoGUI;

    /**
     * the parcial calification of files
     */
    private double noGUIPartialCalification;

    /**
     * List of file to analyse
     */
    List<File> filesInFolder;
    
    /**
     * corresponding position in the scores array of the score obtain on each
     * metric
     */
    @Resource
    private static final int[] TABLESCORES = new int[]{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
    
    /**
     * corresponding position in the finalScores array of the scores obtain on
     * each metric
     */
    @Resource
    private static final int[] FINALTABLESCORES = new int[]{5, 6, 7, 1, 2, 0, 3, 4, 8, 9};

    /**
     * the string resources i18n.
     */
    ResourceBundle messages;

    /**
     *
     * @param pathToAnalyse of the file
     * @param fileName name of the output file
     * @param messages with all Strings variables
     */
    NoGUI(String pathToAnalyse, String fileName, ResourceBundle messages) {
        try {
            this.scoresNoGUI = new ArrayList<>();
            this.scoresImplicitNoneNoGUI = new ArrayList<>();
            this.scoresRatioNoGUI = new ArrayList<>();
            this.scoresNestedLoopsNoGUI = new ArrayList<>();
            this.scoresCommentsBeginningNoGUI = new ArrayList<>();
            this.scoresCommentsVariablesNoGUI = new ArrayList<>();
            this.scoresCommentsfunctionNoGUI = new ArrayList<>();
            this.scoresCommentsSubroutineNoGUI = new ArrayList<>();
            this.scoresCommentsControlStructuresNoGUI = new ArrayList<>();
            this.scoresExitNoGUI = new ArrayList<>();
            this.scoresCycleNoGUI = new ArrayList<>();
            this.filesInFolder = new ArrayList<>();
            this.commentableElementsNoGUI = 0.0;
            this.commentedElementsNoGUI = 0.0;
            this.noGUIPartialCalification = 0.0;

            this.messages = messages;
            this.dirPath = pathToAnalyse;

            System.out.println("messages");
            System.out.println("NoGUI.DEST_PATH");
            System.out.println("pathToAnalyse");
            System.out.println(messages);
            System.out.println(NoGUI.DEST_PATH);
            System.out.println(pathToAnalyse);

            this.analyseFiles(NoGUI.DEST_PATH + fileName + ".pdf");
            
        } catch (IOException ex) {
            Logger.getLogger(NoGUI.class.getName()).log(Level.SEVERE, null, ex);
        }

    }


    /**
     * In background, this method obtains all the files in an specific directory
     * and subdirectories and analyse them where the extension of the files are
     * ".f90" or ".h90" or ".f"
     * 
     * @param filePath the path of the file to analyse
     * @throws IOException in case that the reading file are wrong
     */
    private void analyseFiles(String filePath) throws IOException {
        /**
         * initialice all arrayList and global variables
         */   
        this.scoresImplicitNoneNoGUI.clear();
        this.scoresRatioNoGUI.clear();
        this.scoresNestedLoopsNoGUI.clear();
        this.scoresCommentsBeginningNoGUI.clear();
        this.scoresCommentsVariablesNoGUI.clear();
        this.scoresCommentsfunctionNoGUI.clear();
        this.scoresCommentsSubroutineNoGUI.clear();
        this.scoresCommentsControlStructuresNoGUI.clear();
        this.scoresExitNoGUI.clear();
        this.scoresCycleNoGUI.clear();
        this.filesInFolder.clear();
        this.commentableElementsNoGUI = 0.0;
        this.commentedElementsNoGUI = 0.0;
        this.totalNumLinesNoGUI = 0;
        this.noGUIPartialCalification = 0.0;
        this.assesment = 0.0;
       

        PDF pdf;
        double auxNote = 0.0;

        try {

            String auxDir = "";
            pdf = new PDF();
            String extensionFile = "";
            

            /**
             * In case the temp folder doesn't exits
             */
            if (!Paths.get(filePath).toFile().exists()) {
                new File(NoGUI.DEST_PATH).mkdirs();
            }

            
            pdf.createPdf(filePath, this.messages.getLocale());

            TasksBar.scanFilesInDirectory(this.dirPath, filesInFolder);

            /**
             * for each file of the directory and subdirectory
             */
            for (File file : filesInFolder) {

                this.scoresNoGUI.clear();
                extensionFile = TasksBar.getFileExtension(file).toLowerCase();

                /**
                 * Check if the file is not empty
                 */
                if (file.length() > 0) {
                    /**
                     * If it is a new directory, the path is added into the
                     * report.
                     */
                    if (!auxDir.equals(TasksBar.getPathFromFile(file))
                            && (extensionFile.equals(TasksBar.getEXTENSION())
                            || extensionFile.equals(TasksBar.getEXTENSION2())
                            || extensionFile.equals(TasksBar.getEXTENSION3()))) {
                        auxDir = TasksBar.getPathFromFile(file);
                        pdf.addSection(auxDir);
                    }

                    /**
                     * If it is a new file of fortran code, the path is added
                     * into the report.
                     */
                    if (extensionFile.equals(TasksBar.getEXTENSION())
                            || extensionFile.equals(TasksBar.getEXTENSION2())
                            || extensionFile.equals(TasksBar.getEXTENSION3())) {
                        pdf.addSubSection(file.getName());
                        pdf.addResult(analyseFile(file.getAbsolutePath()));
                        pdf.addTableScores(scoresNoGUI, this.messages,13,1, NoGUI.TABLESCORES);
                        pdf.addScoreResult(this.messages.getString("noteFile") + String.format("%.3f", assesment));
                    }

                }
            }

            /**
             * the list scores is reused to stock the average of all metrics.
             */
            this.scoresNoGUI.clear();
            this.scoresNoGUI.add(TasksBar.calculateAverage(this.scoresImplicitNoneNoGUI));
            this.scoresNoGUI.add(TasksBar.calculateAverage(this.scoresRatioNoGUI));
            this.scoresNoGUI.add(TasksBar.calculateAverage(this.scoresNestedLoopsNoGUI));
            this.scoresNoGUI.add(TasksBar.calculateAverage(this.scoresCommentsBeginningNoGUI));
            this.scoresNoGUI.add(TasksBar.calculateAverage(this.scoresCommentsVariablesNoGUI));
            this.scoresNoGUI.add(TasksBar.calculateAverage(this.scoresCommentsfunctionNoGUI));
            this.scoresNoGUI.add(TasksBar.calculateAverage(this.scoresCommentsSubroutineNoGUI));
            this.scoresNoGUI.add(TasksBar.calculateAverage(this.scoresCommentsControlStructuresNoGUI));
            this.scoresNoGUI.add(TasksBar.calculateAverage(this.scoresExitNoGUI));
            this.scoresNoGUI.add(TasksBar.calculateAverage(this.scoresCycleNoGUI));

            /**
             * Check if the software analysed have not Fortran files
             */
            if (!this.scoresNoGUI.get(0).isNaN()) {
                pdf.addSection(this.messages.getString("finalTable"));
                pdf.addTableScores(this.scoresNoGUI, this.messages,15,0,NoGUI.FINALTABLESCORES);
                auxNote = noGUIPartialCalification / this.totalNumLinesNoGUI;
                pdf.addFinalNote(this.messages.getString("arithmeticAverage") + " " + String.format(Locale.ROOT, "%.3f", auxNote));
            }

            pdf.closePDF();
            filesInFolder.clear();

            this.noGUIPartialCalification = 0.0;
            this.totalNumLinesNoGUI = 0;

            System.exit(1);

        } catch (IOException ex) {
            Logger.getLogger(MainWindow.class.getName()).log(Level.SEVERE, null, ex);
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
        int numLines = TasksBar.analyseNumberOfLines(pathFile);
        boolean useImplicitNone = TasksBar.analyseUseImplicitNone(pathFile);
        boolean checkNestedLoops = TasksBar.analyseNestedLoops(pathFile);
        boolean useExit = TasksBar.analyseUseExit(pathFile);
        boolean useCycle = TasksBar.analyseUseCycle(pathFile);
        int numFunctions = TasksBar.analyseNumFunctions(pathFile);
        int numSubroutines = TasksBar.analyseNumberSubroutines(pathFile);
        int numVariables = TasksBar.analyseNumberOfDeclaredVariables(pathFile);
        String goodComments = this.analyseGoodComment(pathFile);
        
        this.commentableElementsNoGUI += numFunctions;
        this.commentableElementsNoGUI += numSubroutines;
        this.commentableElementsNoGUI += numVariables;

        /**
         * count the number of lines in the file
         */
        result += this.messages.getString("numberOfLines") + numLines;
        result += "\n";

        this.totalNumLinesNoGUI += numLines;

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
            this.scoresNoGUI.add(2.0);
            this.scoresImplicitNoneNoGUI.add(2.0);

        } else {
            this.scoresNoGUI.add(0.0);
            this.scoresImplicitNoneNoGUI.add(0.0);
        }

        /**
         * count the number of functions declared
         */
        result += this.messages.getString("numFunctions") + numFunctions;
        result += "\n";

        /**
         * count the number of subroutines calls
         */
        result += this.messages.getString("subroutinesCall") + TasksBar.analyseNumCalls(pathFile);
        result += "\n";

        /**
         * 7. calcule the ratio and show it in percentage in the report
         */       
        if (commentableElementsNoGUI > 0.0) {
            ratio = (commentedElementsNoGUI / commentableElementsNoGUI);
        }

        result += this.messages.getString("ratio") + String.format(Locale.ROOT, "%.2f", (ratio * 100)) + "%";
        result += " \n";

        ratio = ratio * 2.0;

        assesment += ratio;
        this.scoresNoGUI.add(ratio);
        this.scoresRatioNoGUI.add(ratio);

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
         * in case the nestedLoop are okay
         */
        if (checkNestedLoops) {
            assesment += 2.0;
            this.scoresNoGUI.add(2.0);
            this.scoresNestedLoopsNoGUI.add(2.0);
        } else {
            this.scoresNoGUI.add(0.0);
            this.scoresNestedLoopsNoGUI.add(0.0);
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
            this.scoresNoGUI.add(1.0);
            this.scoresExitNoGUI.add(1.0);
        } else {
            this.scoresNoGUI.add(0.0);
            this.scoresExitNoGUI.add(0.0);
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
            this.scoresNoGUI.add(1.0);
            this.scoresCycleNoGUI.add(1.0);
        } else {
            this.scoresNoGUI.add(0.0);
            this.scoresCycleNoGUI.add(0.0);
        }

        this.noGUIPartialCalification += this.assesment * numLines;

        return result;

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
    private String analyseGoodComment(String filePath) throws IOException {

        String sb = "";
        boolean goodCommentFunctions = this.analyseGoodCommentFunctions(filePath);
        boolean goodCommentInitDoc = TasksBar.analyseGoodCommentInitDoc(filePath);
        boolean goodCommentVariables = this.analyseGoodCommentedVariables(filePath);
        boolean goodCommentSubroutines = this.analyseGoodCommentSubroutines(filePath);
        boolean goodCommentControlStructures = TasksBar.analyseGoodCommentControlStructures(filePath);

        /**
         * good comment in functions
         */
        sb = TasksBar.getARROW() + this.messages.getString("function") + goodCommentFunctions;

        /**
         * good comment at the begining of the document
         */
        sb += TasksBar.getARROW() + this.messages.getString("initDoc") + goodCommentInitDoc;

        /**
         * good comment at variables declaration
         */
        sb += TasksBar.getARROW() + this.messages.getString("variables") + goodCommentVariables;

        /**
         * good comment soubroutines declaration
         */
        sb += TasksBar.getARROW() + this.messages.getString("commentSubroutines") + goodCommentSubroutines;

        /**
         * good comment in control structures
         */
        sb += TasksBar.getARROW() + this.messages.getString("commentControlStructures") + goodCommentControlStructures;

        /**
         * 1. comments in functions
         */
        if (goodCommentFunctions) {
            assesment += 0.4;
            this.scoresNoGUI.add(0.4);
            this.scoresCommentsfunctionNoGUI.add(0.4);
        } else {
            this.scoresNoGUI.add(0.0);
            this.scoresCommentsfunctionNoGUI.add(0.0);
        }
        /**
         * 2. comments at the begining of the document
         */
        if (goodCommentInitDoc) {
            assesment += 0.4;
            this.scoresNoGUI.add(0.4);
            this.scoresCommentsBeginningNoGUI.add(0.4);
        } else {
            this.scoresNoGUI.add(0.0);
            this.scoresCommentsBeginningNoGUI.add(0.0);
        }
        /**
         * 3. comments in variables
         */
        if (goodCommentVariables) {
            assesment += 0.4;
            this.scoresNoGUI.add(0.4);
            this.scoresCommentsVariablesNoGUI.add(0.4);
        } else {
            this.scoresNoGUI.add(0.0);
            this.scoresCommentsVariablesNoGUI.add(0.0);
        }
        /**
         * 4. comments in subroutines
         */
        if (goodCommentSubroutines) {
            assesment += 0.4;
            this.scoresNoGUI.add(0.4);
            this.scoresCommentsSubroutineNoGUI.add(0.4);
        } else {
            this.scoresNoGUI.add(0.0);
            this.scoresCommentsSubroutineNoGUI.add(0.0);
        }
        /**
         * 5. comments in control structures
         */
        if (goodCommentControlStructures) {
            assesment += 0.4;
            this.scoresNoGUI.add(0.4);
            this.scoresCommentsControlStructuresNoGUI.add(0.4);
        } else {
            this.scoresNoGUI.add(0.0);
            this.scoresCommentsControlStructuresNoGUI.add(0.0);
        }

        return sb;

    }

    /**
     * This method analyse if the declaration of subroutines are commented
     *
     * @param filePath of the file analysed
     * @return boolean with the result of the good comments in subroutines
     * @throws IOException in case something wrong with intput/output file
     */
    private boolean analyseGoodCommentSubroutines(String filePath) throws IOException {

        String line = "";
        String previousLine = "";
        File file = new File(filePath);
        int numSubroutines = 0;
        int totalSubroutines = 0;

        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((line = b.readLine()) != null) {

                line = line.toUpperCase();

                //check if it is a declaration of a function
                if (!line.contains("!")
                        && !line.contains("END SUBROUTINE")
                        && line.contains("SUBROUTINE")) {
                    totalSubroutines++;

                    //check if the next line is a comment or the previous line
                    //is a comment
                    if (previousLine.contains("!")) {
                        numSubroutines++;
                        this.commentedElementsNoGUI++;
                    }
                }
                previousLine = line;
            }
        }
        return totalSubroutines == numSubroutines;
    }

    /**
     * This method analyse if for each variable, there is a comment to describe
     * what it done.
     *
     * @param filePath of the file analysed
     * @return boolean with the result of the use of good comments in variables
     * @throws IOException in case something wrong with intput/output file
     */
    private boolean analyseGoodCommentedVariables(String filePath) throws IOException {
        String chain = "";
        String previousChain = "";
        File file = new File(filePath);
        int variablesCommented = 0;
        int totalVariables = 0;

        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {

                //check if the chain is a declaration of a variable and
                //it is not a comment 
                if (chain.contains("::")) {

                    totalVariables++;

                    //check if the next line is a comment or the previous line
                    //is a comment
                    if (previousChain.contains("!")) {
                        variablesCommented++;
                        this.commentedElementsNoGUI++;
                    }

                }
                previousChain = chain;
            }
        }
        return totalVariables == variablesCommented;
    }

    /**
     * This method check if the functions delcared in a file have or not have a
     * comment. The comment can be after or before the declaration of the
     * function. In addition, at the end of functions there are no comments.
     *
     * @param filePath of the analysed file
     * @return boolean with the result of the use of good comments in functions
     * @throws IOException in case something wrong with intput/output file
     */
    private boolean analyseGoodCommentFunctions(String filePath) throws IOException {

        String characters = "";
        String previousCharacters = "";
        File file = new File(filePath);
        int numFunction = 0;
        int totalFunctions = 0;

        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((characters = b.readLine()) != null) {

                characters = characters.toUpperCase();
                //check if it is a declaration of a function
                if (!characters.contains("!")
                        && !characters.contains("END FUNCTION")
                        && characters.contains("FUNCTION")) {
                    totalFunctions++;

                    //check if the next line is a comment or the previous line
                    //is a comment
                    if (previousCharacters.contains("!")) {
                        numFunction++;
                        this.commentedElementsNoGUI++;
                    }
                }
                previousCharacters = characters;
            }
        }
        return totalFunctions == numFunction;
    }
    
    
    /**
     * Getter from tablesScores
     * @return array from tablesScores
     */
    public static int[] getTablesScores(){
        return TABLESCORES;
    }
    
    /**
     * Getter from finalTablesScores
     * @return array from finalTablesScores
     */
    public static int[] getFinalTablesScores(){
        return FINALTABLESCORES;
    }
    
}
