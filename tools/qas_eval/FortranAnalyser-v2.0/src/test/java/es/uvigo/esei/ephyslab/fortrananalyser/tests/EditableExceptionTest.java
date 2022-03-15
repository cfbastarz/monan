/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package es.uvigo.esei.ephyslab.fortrananalyser.tests;

import es.uvigo.esei.ephyslab.fortrananalyser.EditableException;
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
public class EditableExceptionTest {

    public EditableExceptionTest() {
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

    @Test
    public void testNoEditableException() {
        String[] args = {"es", "quality.pdf", "ex"};
        boolean toret = false;

        if (args.length == 3) {
            toret = true;
        }

        assertTrue(toret);

    }

    @Test(expected = EditableException.class)
    public void testEditableException() throws EditableException, Exception {
        String[] args = {"es", "quality.pdf"};

        if (args.length != 3) {
            throw new EditableException(111);
        }
        
    }
}
