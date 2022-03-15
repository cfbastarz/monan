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
import java.util.ArrayList;
import java.util.ResourceBundle;

/**
 *
 * @author Michael García Rodríguez
 */
public class CycloComplexity {

    private final ArrayList<Double> scoresCC;

    /**
     * empty constructor.
     */
    public CycloComplexity() {
        this.scoresCC = new ArrayList<>();
    }

    /**
     * This method calculate the cyclomatic complexity of a file from path
     * variable. This calcule is found in many tools of other programming
     * languages
     *
     * @param path the path of the file to be analysed.
     * @param messages list of messages in all available languages
     * @return the string with the result
     * @throws java.io.IOException in case that file is not opened.
     */
    public String calculateComplexitySimpleCalcule(String path, ResourceBundle messages) throws IOException {

        String chain = "";
        String result = "";
        String nameFunctSub = "";
        int cc = 1;
        this.scoresCC.clear();
        boolean method = false;
        File file = new File(path);

        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {
                chain = chain.toUpperCase();

                if (!chain.contains("!")
                        && ((!chain.contains("END FUNCTION") && chain.contains("FUNCTION ")) || (!chain.contains("END SUBROUTINE") && chain.contains("SUBROUTINE ")))
                        && !method) {
                    nameFunctSub = chain;
                    method = true;
                }

                /**
                 * checking the words contains in the line to add complexity.
                 */
                if (method) {
                    if (!chain.contains("!") && !chain.contains("ENDIF") && (chain.contains(".AND.")
                            || chain.contains(".OR.")
                            || chain.contains("WHILE")
                            || chain.contains("GO TO")
                            || chain.contains("CONTINUE")
                            || chain.contains("DEFAULT")
                            || chain.contains("CASE")
                            || chain.contains("IF"))) {

                        cc++;
                    }

                    if (!chain.contains("!") && (chain.contains("END FUNCTION") || chain.contains("END SUBROUTINE"))) {

                        result += nameFunctSub + ": " + cc;
                        result += "\n";
                        this.scoresCC.add(new Double(cc));
                        cc = 1;
                        method = false;
                    }

                }

            }
        }

        return result;
    }

    public ArrayList<Double> getScoresCC() {
        return scoresCC;
    }
}
