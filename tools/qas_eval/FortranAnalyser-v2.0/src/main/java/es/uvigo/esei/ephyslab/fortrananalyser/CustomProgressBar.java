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

import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.geom.Arc2D;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Rectangle2D;
import javax.swing.JPanel;

/**
 * This class customize a rounded progress bar with the percentage in the center
 * of it.
 *
 * @author Michael García Rodríguez
 */
public class CustomProgressBar extends JPanel {

    int progressValue;

    public void updateProgressBar(int progress_value) {
        this.progressValue = progress_value;
    }

    /**
     * Define the dimensions and location of the custom progress bar overriding
     * the paint function.
     *
     * @param g the graphic to print
     */
    @Override
    public void paint(Graphics g) {
        super.paint(g);

        Graphics2D g2 = (Graphics2D) g;
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

        g2.translate(this.getWidth() / 2, this.getHeight() / 2);
        g2.rotate(Math.toRadians(270));

        Arc2D.Float arc = new Arc2D.Float(Arc2D.PIE);

        Ellipse2D circle = new Ellipse2D.Double(0, 0, 40, 40);
        arc.setFrameFromCenter(new Point(0, 0), new Point(50, 50));
        circle.setFrameFromCenter(new Point(0, 0), new Point(40, 40));

        arc.setAngleStart(1);
        arc.setAngleExtent(-progressValue * 3.6);
        g2.setColor(new Color(61, 143, 199));
        g2.draw(arc);
        g2.fill(arc);

        g2.setColor(new Color(234, 234, 234));
        g2.draw(circle);
        g2.fill(circle);

        g2.setColor(new Color(61, 143, 199));

        g2.rotate(Math.toRadians(90));
        g2.setFont(new Font(TOOL_TIP_TEXT_KEY, Font.PLAIN, 25));

        FontMetrics fm = g2.getFontMetrics();
        Rectangle2D r = fm.getStringBounds(progressValue + "%", g);

        int x = (0 - (int) r.getWidth() / 2);
        int y = (0 - (int) r.getHeight() / 2 + fm.getAscent());

        g2.drawString(progressValue + "%", x, y);
    }

}
