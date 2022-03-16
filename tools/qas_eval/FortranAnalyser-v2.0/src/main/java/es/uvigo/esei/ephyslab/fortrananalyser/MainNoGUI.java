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

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Paths;
import java.util.Locale;
import java.util.ResourceBundle;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Michael García Rodríguez
 * @version 2.0
 */
public class MainNoGUI {

    /**
     * the string resource of the application.
     */
    ResourceBundle messages;

    /**
     * the name of the package where the messagesBundle i18n are.
     */
    static final String BUNDLE = "es.uvigo.esei.ephyslab.i18n.bundle.MessagesBundle";

    /**
     * the path of the readme.txt file.
     */
    static final String USERMANUAL = "userManual";

    /**
     * the path of the license.txt file.
     */
    static final String PATHLICENSE = "license.pdf";

    /**
     * the path of the errorpdf.pdf file.
     */
    static final String PATHERRORPDF = "errorpdf.pdf";

    /**
     * the local path of the quality report file in local home directory of the
     * current user.
     */
    static final String QUALITYREPORT = System.getProperty("user.home") + "/temp/QualityReport.pdf";

    /**
     * other available languages to the user interface.
     */
    static final String[] AVAILABLE_LANGUAGES = {"es", "fr", "gl", "en", "pt"};

    /**
     * other available countries
     */
    static final String[] AVAILABLE_COUNTRIES = {"ES", "FR", "ES", "GB", "PT"};

    /**
     * By default, the selected language is Enslish.
     */
    Locale currentLocale;

    /**
     * default country to initialice the user interface.
     */
    static final String DEFAULT_COUNTRY = "GB";

    /**
     * default language to initialice the user interface.
     */
    static final String DEFAULT_LANGUAGE = "en";

    /**
     * default typography of the the user interface.
     */
    static final String UBUNTU = "Ubuntu";


    /**
     * Constructor for NoGUI usage
     *
     * @param language selected
     * @param pathToAnalyse path to analyse
     * @param fileName name of the output file
     */
    public MainNoGUI(String language, String pathToAnalyse, String fileName) {
        initialiceComponentsNoGUI();
        this.changeLanguage(language);
//        new NoGUI(pathToAnalyse, fileName, MainNoGUI.this.messages);
        System.out.println("Generating " + fileName + " from source: " + pathToAnalyse + " ...");

        TasksBarNoGUI t = new TasksBarNoGUI(MainNoGUI.this, pathToAnalyse, MainNoGUI.this.messages, fileName);
        try {
            t.doInBackground();
        } catch (Exception e) {
            System.out.println("ERROR generating: \n" );
            e.printStackTrace(System.out);
        } 
        System.out.println("Done!");


    }

    /**
     * This method update the parameters in the lenguage selected by the user.
     * es: spanish, fr: french, gl: galician, en: english
     *
     * @param lang the lenguage selected
     */
    private void changeLanguage(String lang) {

        for (int i = 0; i < AVAILABLE_LANGUAGES.length; i++) {

            if (AVAILABLE_LANGUAGES[i].equals(lang)) {
                currentLocale = new Locale(AVAILABLE_LANGUAGES[i], AVAILABLE_COUNTRIES[i]);
                this.messages = ResourceBundle.getBundle(MainNoGUI.BUNDLE, currentLocale);
                break;
            }
        }
    }


    /**
     * Components to use in case the programme is lanched by console
     */
    private void initialiceComponentsNoGUI() {

        this.currentLocale = new Locale(MainNoGUI.DEFAULT_LANGUAGE, MainNoGUI.DEFAULT_COUNTRY);
        Locale.setDefault(currentLocale);
        this.messages = ResourceBundle.getBundle(MainNoGUI.BUNDLE, currentLocale);

    }

}
