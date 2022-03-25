/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package es.uvigo.esei.ephyslab.fortrananalyser.tests;

import es.uvigo.esei.ephyslab.fortrananalyser.NoGUI;
import es.uvigo.esei.ephyslab.fortrananalyser.TasksBar;
import java.util.ArrayList;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author Michael García Rodríguez
 */

public class NoGUIJUnitTest {
    
    
    public NoGUIJUnitTest() {
    }
    
    @BeforeClass
    public static void setUpClass() {
    }
    
    @AfterClass
    public static void tearDownClass() {
    }
    
    @Before
    public void setUp() {
    }
    
    @After
    public void tearDown() {
    }

    // TODO add test methods here.
    // The methods must be annotated with annotation @Test. For example:
    //
    
  
    @Test
    public void testDEST_PATH(){
        String path =System.getProperty("user.home") + "/temp";
        assertEquals(NoGUI.DEST_PATH, path);
    }
    
    @Test
    public void testTABLESCORES(){
        int[] expected = new int[]{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
        int[] obtain = NoGUI.getTablesScores();
        
        assertArrayEquals(expected, obtain);
    }
    
    @Test
    public void testFINALTABLESCORES(){
        int[] expected = new int[]{5, 6, 7, 1, 2, 0, 3, 4, 8, 9};
        int[] obtain = NoGUI.getFinalTablesScores();
        
        assertArrayEquals(expected,obtain);
    }
    
    @Test
    public void testCalculateAverage() {
        ArrayList<Double> l= new ArrayList<>();
        l.add(1.0);
        l.add(2.0);
        l.add(3.0);
        
        assertEquals(2, TasksBar.calculateAverage(l), 0.01);

    }
    
    @Test
    public void testCalculateAverageZero(){
        ArrayList<Double> l= new ArrayList<>();
        l.add(0.0);
        
        assertEquals(0,TasksBar.calculateAverage(l),0.01);
        
    }
    
    @Test
    public void testCalculateAverageEmpty(){
        ArrayList<Double> l= new ArrayList<>();
        
        assertEquals(0,TasksBar.calculateAverage(l),0.01);
        
    }
    
    
}
