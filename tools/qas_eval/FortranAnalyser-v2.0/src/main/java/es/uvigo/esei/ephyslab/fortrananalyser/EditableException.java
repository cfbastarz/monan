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

/**
 * Exception class with all EditableException
 *
 * @author Michael García Rodríguez
 * @version 1.9.8
 */
public class EditableException extends Exception {

    /**
     * code number of the exception error
     */
    private final int errorCode;

    public EditableException(int codigoError) {
        super();
        this.errorCode = codigoError;
    }

    @Override
    public String getMessage() {

        String errorMessage = "";

        if (errorCode == 111) {
            errorMessage = "bad number of argument. Check README.md file \n";
        } else {
            errorMessage = "Generic Exception Error \n";
        }

        return errorMessage;

    }

}
