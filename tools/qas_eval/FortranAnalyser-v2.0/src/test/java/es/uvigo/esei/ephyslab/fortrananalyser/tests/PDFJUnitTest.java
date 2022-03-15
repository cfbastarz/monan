/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package es.uvigo.esei.ephyslab.fortrananalyser.tests;

import com.itextpdf.kernel.color.DeviceRgb;
import es.uvigo.esei.ephyslab.fortrananalyser.PDF;
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
public class PDFJUnitTest {
    
    public PDFJUnitTest() {
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
    // @Test
    // public void hello() {}
    
    @Test
    public void testHEADERCOLOR(){
        assertEquals(new DeviceRgb(0, 130, 130), PDF.getHEADERCOLOR());
    }
    
    @Test
    public void testHEADER2COLOR(){
        assertEquals(new DeviceRgb(0, 69, 69), PDF.getHEADER2COLOR());
    }
    
    @Test
    public void testFinalnotecolor(){
        assertEquals(new DeviceRgb(77, 135, 133), PDF.getFINALNOTECOLOR());
    }
    
    @Test
    public void testIcon(){
        assertEquals(PDF.class.getResource("fortranAnalyser.png").toString(), PDF.getICONFORTRANANALYSER());
    }
    
    @Test
    public void testResultCOLOR(){
        assertEquals(new DeviceRgb(38, 50, 61), PDF.getRESULTCOLOR());
    }
    
    @Test
    public void testSectionCOLOR(){
        assertEquals(new DeviceRgb(207, 106, 11), PDF.getSECTIONCOLOR());
    }
    
    @Test
    public void testSubsectionCOLOR(){
        assertEquals(new DeviceRgb(11, 136, 207), PDF.getSUBSECTIONCOLOR());
    }
}
