/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package es.uvigo.esei.ephyslab.fortrananalyser.tests;

import es.uvigo.esei.ephyslab.fortrananalyser.TasksBar;
import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.logging.Level;
import java.util.logging.Logger;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

/**
 *
 * @author Michael García Rodríguez
 */
public class TasksBarJUnitTests {

    private File file;
    private TasksBar tb;

    public TasksBarJUnitTests() {
        try {
            file = new File(getClass().getClassLoader().getResource("files/DISTANCE.f90").toURI());
            tb = new TasksBar();
        } catch (URISyntaxException ex) {
            Logger.getLogger(TasksBarJUnitTests.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    @Test
    public void TestCalculateAverage() {

        int average = 6 / 2;

        assertEquals(3, average);
    }

    @Test
    public void TestCheckFileExist() {

        assertTrue(file.exists());

    }

    @Test
    public void TestAnalyseNumberOfLines() {

        int numLines = 0;

        try {
            numLines = TasksBar.analyseNumberOfLines(file.getPath());
        } catch (IOException ex) {
            Logger.getLogger(TasksBarJUnitTests.class.getName()).log(Level.SEVERE, null, ex);
        }

        assertEquals(38, numLines);
    }

    @Test
    public void TestAnalyseUseImplicitNone() {

        try {

            assertTrue(TasksBar.analyseUseImplicitNone(file.getPath()));

        } catch (IOException ex) {
            Logger.getLogger(TasksBarJUnitTests.class.getName()).log(Level.SEVERE, null, ex);
        }

    }

    @Test
    public void TestAnalyseNumFunctions() {

        int numFunctions = 0;

        try {

            numFunctions = TasksBar.analyseNumFunctions(file.getPath());

        } catch (IOException ex) {
            Logger.getLogger(TasksBarJUnitTests.class.getName()).log(Level.SEVERE, null, ex);
        }

        assertEquals(0, numFunctions);
    }

    @Test
    public void TestAnalyseNumCalls() {

        int numCalls = 0;

        try {

            numCalls = TasksBar.analyseNumCalls(file.getPath());

        } catch (IOException ex) {
            Logger.getLogger(TasksBarJUnitTests.class.getName()).log(Level.SEVERE, null, ex);
        }

        assertEquals(0, numCalls);
    }

    @Test
    public void TestAnalyseUseCycle() {

        try {

            assertFalse(TasksBar.analyseUseCycle(file.getPath()));

        } catch (IOException ex) {
            Logger.getLogger(TasksBarJUnitTests.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    @Test
    public void TestAnalyseUseExit() {

        try {

            assertFalse(TasksBar.analyseUseExit(file.getPath()));

        } catch (IOException ex) {
            Logger.getLogger(TasksBarJUnitTests.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    @Test
    public void TestAnalyseNumberSubroutines() {

        int numSubroutines = 0;

        try {

            numSubroutines = TasksBar.analyseNumberSubroutines(file.getPath());

        } catch (IOException ex) {
            Logger.getLogger(TasksBarJUnitTests.class.getName()).log(Level.SEVERE, null, ex);
        }

        assertEquals(0, numSubroutines);
    }

    @Test
    public void TestAnalyseNestedLoops() {

        try {

            assertTrue(TasksBar.analyseNestedLoops(file.getPath()));

        } catch (IOException ex) {
            Logger.getLogger(TasksBarJUnitTests.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    @Test
    public void TestAnalyseNumberOfDeclaredVariables() {

        int numDeclaredVariables = 0;

        try {

            numDeclaredVariables = TasksBar.analyseNumberOfDeclaredVariables(file.getPath());

        } catch (IOException ex) {
            Logger.getLogger(TasksBarJUnitTests.class.getName()).log(Level.SEVERE, null, ex);
        }

        assertEquals(18, numDeclaredVariables);
    }

    @Test
    public void TestAnalyseGoodCommentControlStructures() {

        try {

            assertFalse(TasksBar.analyseGoodCommentControlStructures(file.getPath()));

        } catch (IOException ex) {
            Logger.getLogger(TasksBarJUnitTests.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    @Test
    public void TestAnalyseGoodCommentSubroutines() {

        try {

            assertTrue(tb.analyseGoodCommentSubroutines(file.getPath()));

        } catch (IOException ex) {
            Logger.getLogger(TasksBarJUnitTests.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    @Test
    public void TestAnalyseGoodCommentVariables() {

        try {

            assertFalse(tb.analyseGoodCommentedVariables(file.getPath()));

        } catch (IOException ex) {
            Logger.getLogger(TasksBarJUnitTests.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    @Test
    public void TestAnalyseGoodCommentInitDoc() {

        try {

            assertFalse(TasksBar.analyseGoodCommentInitDoc(file.getPath()));

        } catch (IOException ex) {
            Logger.getLogger(TasksBarJUnitTests.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    @Test
    public void TestAnalyseGoodCommentFunctions() {

        try {

            assertFalse(tb.analyseGoodCommentFunctions(file.getPath()));

        } catch (IOException ex) {
            Logger.getLogger(TasksBarJUnitTests.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    @Test(expected = IllegalArgumentException.class)
    public void TestGetDurationAnalyseException() throws Exception {

        TasksBar.getDurationAnalyse(-1);

    }

    @Test
    public void TestGetDurationAnalyse() {

        String expectedDate = "17 D 10 h 41 min 23 s 23700 ms";

        assertEquals(expectedDate, TasksBar.getDurationAnalyse(1507283723));
    }

    @Test
    public void testDEST() {
        String expected = System.getProperty("user.home") + "/temp/QualityReport.pdf";
        String obtain = TasksBar.getDEST();

        assertEquals(expected, obtain);
    }

    @Test
    public void testDEST_PATH() {
        String expected = System.getProperty("user.home") + "/temp";
        String obtain = TasksBar.getDESTPATH();

        assertEquals(expected, obtain);
    }

    @Test
    public void testPOSITIONTABLESCORES() {
        int[] expected = new int[]{5, 6, 7, 1, 2, 0, 3, 4, 8, 9};
        int[] obtain = TasksBar.getPOSITIONTABLESCORES();

        assertArrayEquals(expected, obtain);
    }

    @Test
    public void testPOSITIONSFINALTABLESCORES() {
        int[] expected = new int[]{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
        int[] obtain = TasksBar.getPOSITIONSFINALTABLESCORES();

        assertArrayEquals(expected, obtain);
    }

    @Test
    public void testEmptyConstructor() {
        assertNotNull(new TasksBar());

    }

    @Test
    public void testGetPathFromFile() {

        assertEquals(System.getProperty("user.dir") + "/target/test-classes/files", TasksBar.getPathFromFile(file));
    }

    @Test
    public void testGetFileExtension() {

        assertEquals("f90", TasksBar.getFileExtension(file));
    }

    @Test
    public void testAnalyseFile() throws IOException {

        try {
            assertEquals("example", tb.analyseFile("files/DISTANCE.F90"));
        } catch (IOException ex) {
            Logger.getLogger(TasksBarJUnitTests.class.getName()).log(Level.SEVERE, null, ex);
        }

    }

    @Test(expected = IOException.class)
    public void testAnalyseWrongFile() throws IOException {

        assertEquals("example", tb.analyseFile("files/DISTANCE2.F90"));

    }

    @Test
    public void testAnalyseNumberOfLines() throws IOException {
        
        assertEquals(38,TasksBar.analyseNumberOfLines(file.getAbsolutePath()));
    }
    
    @Test
    public void testAnalyseUseImplicitNone() throws IOException{
        
        assertTrue(TasksBar.analyseUseImplicitNone(file.getAbsolutePath()));
    }
    
    @Test
    public void testAnalyseNumFunctions() throws IOException{
        
        assertEquals(0,TasksBar.analyseNumFunctions(file.getAbsolutePath()));
    }
    
    @Test
    public void testAnalyseNumCalls() throws IOException{
        
        assertEquals(0,TasksBar.analyseNumCalls(file.getAbsolutePath()));
    }
    
   @Test
   public void testAnalyseUseCycle() throws IOException{
       
       assertFalse(TasksBar.analyseUseCycle(file.getAbsolutePath()));
   }
   
   @Test
   public void testAnalyseUseExit() throws IOException{
       
       assertFalse(TasksBar.analyseUseExit(file.getAbsolutePath()));
   }
   
   @Test
   public void testAnalyseNumberSubroutines() throws IOException{
       
       assertEquals(0, TasksBar.analyseNumberSubroutines(file.getAbsolutePath()));
   }

   @Test
   public void testAnalyseNestedLoops() throws IOException{
       
       assertTrue(TasksBar.analyseNestedLoops(file.getAbsolutePath()));
   }
   
   @Test
   public void testAnalyseNumberOfDeclaredVariables() throws IOException{
       
       assertEquals(18, TasksBar.analyseNumberOfDeclaredVariables(file.getAbsolutePath()));
   }
      
   @Test
   public void testAnalyseGoodCommentControlStructures() throws IOException{
       
       assertFalse(TasksBar.analyseGoodCommentControlStructures(file.getAbsolutePath()));
   }
   
   @Test
   public void testAnalyseGoodCommentSubroutines() throws IOException{
       
       assertTrue(tb.analyseGoodCommentSubroutines(file.getAbsolutePath()));
   }
   
   @Test
   public void testAnalyseGoodCommentedVariables() throws IOException{
       
       assertFalse(tb.analyseGoodCommentedVariables(file.getAbsolutePath()));
   }
   
   @Test
   public void testAnalyseGoodCommentInitDoc() throws IOException{
       
       assertFalse(TasksBar.analyseGoodCommentInitDoc(file.getAbsolutePath()));
   }
   
   @Test
   public void testAnalyseGoodCommentFunctions() throws IOException{
       
       assertFalse(tb.analyseGoodCommentFunctions(file.getAbsolutePath()));
   }
   
   @Test
   public void testGetDurationAnalyse(){
       
       assertEquals("0 D 0 h 0 min 13 s 13530 ms",TasksBar.getDurationAnalyse(13543));
   }
   
   @Test
   public void testCalculateAverage(){
       ArrayList<Double> l = new ArrayList<> ();
       l.add(1.0);
       l.add(2.0);
       l.add(3.0);
       
       assertEquals(2.0,TasksBar.calculateAverage(l), 0.01);
       
   }
}
