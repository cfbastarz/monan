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

import java.awt.Desktop;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
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
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;

/**
 *
 * @author Michael García Rodríguez
 * @version 2.0
 */
public class MainWindow extends javax.swing.JFrame {

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
     * the file choser from the computer where the application is open.
     */
    private final JFileChooser fc;

    /**
     * Creates new form MainWindow
     */
    public MainWindow() {
        initComponents();
        fc = new JFileChooser();
        initializeVariables();

        this.jLabel8.addMouseListener(new MouseListener() {

            @Override
            public void mouseClicked(MouseEvent e) {
                changeLanguage("en");
                reconfigureComponents();
            }

            @Override
            public void mousePressed(MouseEvent e) {
                //nothing to do

            }

            @Override
            public void mouseReleased(MouseEvent e) {
                //nothing to do
            }

            @Override
            public void mouseEntered(MouseEvent e) {
                jLabel8.setFont(new java.awt.Font(MainWindow.UBUNTU, 1, 15));
            }

            @Override
            public void mouseExited(MouseEvent e) {
                jLabel8.setFont(new java.awt.Font(MainWindow.UBUNTU, 0, 15));
            }
        });

        this.jLabel9.addMouseListener(new MouseListener() {

            @Override
            public void mouseClicked(MouseEvent e) {
                changeLanguage("fr");
                reconfigureComponents();
            }

            @Override
            public void mousePressed(MouseEvent e) {
                //nothing to do
            }

            @Override
            public void mouseReleased(MouseEvent e) {
                //nothing to do
            }

            @Override
            public void mouseEntered(MouseEvent e) {
                jLabel9.setFont(new java.awt.Font(MainWindow.UBUNTU, 1, 15));
            }

            @Override
            public void mouseExited(MouseEvent e) {
                jLabel9.setFont(new java.awt.Font(MainWindow.UBUNTU, 0, 15));
            }
        });

        this.jLabel10.addMouseListener(new MouseListener() {

            @Override
            public void mouseClicked(MouseEvent e) {
                changeLanguage("es");
                reconfigureComponents();
            }

            @Override
            public void mousePressed(MouseEvent e) {
                //nothing to do
            }

            @Override
            public void mouseReleased(MouseEvent e) {
                //nothing to do
            }

            @Override
            public void mouseEntered(MouseEvent e) {
                jLabel10.setFont(new java.awt.Font(MainWindow.UBUNTU, 1, 15));
            }

            @Override
            public void mouseExited(MouseEvent e) {
                jLabel10.setFont(new java.awt.Font(MainWindow.UBUNTU, 0, 15));
            }
        });

        this.jLabel11.addMouseListener(new MouseListener() {

            @Override
            public void mouseClicked(MouseEvent e) {
                changeLanguage("gl");
                reconfigureComponents();
            }

            @Override
            public void mousePressed(MouseEvent e) {
                //nothing to do
            }

            @Override
            public void mouseReleased(MouseEvent e) {
                //nothing to do
            }

            @Override
            public void mouseEntered(MouseEvent e) {
                jLabel11.setFont(new java.awt.Font(MainWindow.UBUNTU, 1, 15));
            }

            @Override
            public void mouseExited(MouseEvent e) {
                jLabel11.setFont(new java.awt.Font(MainWindow.UBUNTU, 0, 15));
            }
        });

        this.jLabel12.addMouseListener(new MouseListener() {

            @Override
            public void mouseClicked(MouseEvent e) {
                openLink(0);
            }

            @Override
            public void mousePressed(MouseEvent e) {
                //nothing to do
            }

            @Override
            public void mouseReleased(MouseEvent e) {
                //nothing to do
            }

            @Override
            public void mouseEntered(MouseEvent e) {
                jLabel12.setFont(new java.awt.Font(MainWindow.UBUNTU, 1, 15));
            }

            @Override
            public void mouseExited(MouseEvent e) {
                jLabel12.setFont(new java.awt.Font(MainWindow.UBUNTU, 0, 15));
            }
        });

        this.jLabel13.addMouseListener(new MouseListener() {

            @Override
            public void mouseClicked(MouseEvent e) {
                openLink(1);
            }

            @Override
            public void mousePressed(MouseEvent e) {
                //nothing to do
            }

            @Override
            public void mouseReleased(MouseEvent e) {
                //nothing to do
            }

            @Override
            public void mouseEntered(MouseEvent e) {
                jLabel13.setFont(new java.awt.Font(MainWindow.UBUNTU, 1, 15));
            }

            @Override
            public void mouseExited(MouseEvent e) {
                jLabel13.setFont(new java.awt.Font(MainWindow.UBUNTU, 0, 15));
            }
        });

        this.jLabel22.addMouseListener(new MouseListener() {
            @Override
            public void mouseClicked(MouseEvent e) {
                openReport();
            }

            @Override
            public void mousePressed(MouseEvent e) {
                //nothing to do
            }

            @Override
            public void mouseReleased(MouseEvent e) {
                //nothing to do
            }

            @Override
            public void mouseEntered(MouseEvent e) {
                jLabel24.setFont(new java.awt.Font(MainWindow.UBUNTU, 1, 10));
            }

            @Override
            public void mouseExited(MouseEvent e) {
                jLabel24.setFont(new java.awt.Font(MainWindow.UBUNTU, 0, 10));
            }
        });

        this.jLabel23.addMouseListener(new MouseListener() {
            @Override
            public void mouseClicked(MouseEvent e) {
                openLink(2);
            }

            @Override
            public void mousePressed(MouseEvent e) {
                //nothing to do
            }

            @Override
            public void mouseReleased(MouseEvent e) {
                //nothing to do
            }

            @Override
            public void mouseEntered(MouseEvent e) {
                //nothing to do
            }

            @Override
            public void mouseExited(MouseEvent e) {
                //nothing to do
            }
        });

        this.jLabel24.addMouseListener(new MouseListener() {
            @Override
            public void mouseClicked(MouseEvent e) {
                openReport();
            }

            @Override
            public void mousePressed(MouseEvent e) {
                //nothing to do
            }

            @Override
            public void mouseReleased(MouseEvent e) {
                //nothing to do
            }

            @Override
            public void mouseEntered(MouseEvent e) {
                jLabel24.setFont(new java.awt.Font(MainWindow.UBUNTU, 1, 10));

            }

            @Override
            public void mouseExited(MouseEvent e) {
                jLabel24.setFont(new java.awt.Font(MainWindow.UBUNTU, 0, 10));
            }
        });

        this.jLabel25.addMouseListener(new MouseListener() {
            @Override
            public void mouseClicked(MouseEvent e) {
                changeLanguage("pt");
                reconfigureComponents();
            }

            @Override
            public void mousePressed(MouseEvent e) {
                //nothing to do
            }

            @Override
            public void mouseReleased(MouseEvent e) {
                //nothing to do
            }

            @Override
            public void mouseEntered(MouseEvent e) {
                jLabel25.setFont(new java.awt.Font(MainWindow.UBUNTU, 1, 15));
            }

            @Override
            public void mouseExited(MouseEvent e) {
                jLabel25.setFont(new java.awt.Font(MainWindow.UBUNTU, 0, 15));
            }
        });

    }

    /**
     * Constructor for NoGUI usage
     *
     * @param language selected
     * @param pathToAnalyse path to analyse
     * @param fileName name of the output file
     */
    public MainWindow(String language, String pathToAnalyse, String fileName) {
        fc = null;
        initialiceComponentsNoGUI();
        this.changeLanguage(language);
        new NoGUI(pathToAnalyse, fileName, MainWindow.this.messages);
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
                this.messages = ResourceBundle.getBundle(MainWindow.BUNDLE, currentLocale);
                break;
            }
        }
    }

    /**
     * Reconfiguration of the buttons in case the language is switched.
     */
    private void reconfigureComponents() {

        this.jTextField1.setText(this.messages.getString("selectDirectory"));
        this.jButton3.setText(this.messages.getString("nameButtonAnalyse"));
        this.jLabel2.setText(this.messages.getString("version"));
        this.jLabel3.setText(this.messages.getString("nameMenu"));
        this.jLabel6.setText(this.messages.getString("about"));
        this.jLabel8.setText(this.messages.getString("nameButtonEnglish"));
        this.jLabel9.setText(this.messages.getString("nameButtonFrench"));
        this.jLabel10.setText(this.messages.getString("nameButtonSpanish"));
        this.jLabel11.setText(this.messages.getString("nameButtonGalician"));
        this.jLabel25.setText(this.messages.getString("nameButtonPortuges"));
        this.jLabel12.setText(this.messages.getString("manual"));
        this.jLabel13.setText(this.messages.getString("license"));
        this.jLabel14.setText(this.messages.getString("errorDirectoryEmpty"));
        this.jLabel15.setText(this.messages.getString("analisisResults"));
        this.jLabel16.setText(this.messages.getString("timeResults"));
        this.jLabel17.setText(this.messages.getString("arithmeticAverage"));
        this.jLabel18.setText(this.messages.getString("directoryMessage"));
        this.jLabel24.setText(this.messages.getString("openFile"));

        /**
         * hide labels of the quality report and the progressBar.
         */
        this.jLabel15.setVisible(false);
        this.jLabel16.setVisible(false);
        this.jLabel17.setVisible(false);
        this.jLabel18.setVisible(false);
        this.jLabel19.setVisible(false);
        this.jLabel20.setVisible(false);
        this.jLabel21.setVisible(false);
        this.jLabel22.setVisible(false);
        this.jLabel24.setVisible(false);
        this.customProgressBar1.setVisible(false);
    }

    /**
     * This method displays the PDF selected by the user.
     *
     * @param num the number to identify the PDF to open: 0 for the user manual;
     * 0: for user manual document; 1: for the license document; 2: open the web
     * page; default: for the eror file;
     */
    private void openLink(int num) {
        Desktop desktop = Desktop.isDesktopSupported() ? Desktop.getDesktop() : null;
        String web = "http://fortrananalyser.ephyslab.uvigo.es/docs/";

        switch (num) {
            case 0:
                web += MainWindow.USERMANUAL + "_" + currentLocale.getLanguage() + ".pdf";
                break;

            case 1:
                web += MainWindow.PATHLICENSE;
                break;

            case 2:
                web = "http://fortrananalyser.ephyslab.uvigo.es/";
                break;

            default:
                web += MainWindow.PATHERRORPDF;
                break;
        }

        if (desktop != null && desktop.isSupported(Desktop.Action.BROWSE)) {

            try {
                desktop.browse(new URI(web));
            } catch (URISyntaxException | IOException ex) {
                Logger.getLogger(MainWindow.class.getName()).log(Level.SEVERE, null, ex);
            }

        }
    }

    /**
     * This method open the report file into the default PDF visor.
     */
    private void openReport() {
        URL url;
        File myFile;

        try {
            url = Paths.get(MainWindow.QUALITYREPORT).toUri().toURL();
            myFile = new File(url.toURI());

            if (Desktop.isDesktopSupported()) {
                Desktop.getDesktop().open(myFile);
            }

        } catch (MalformedURLException | URISyntaxException ex) {
            Logger.getLogger(MainWindow.class.getName()).log(Level.SEVERE, null, ex);
        } catch (IOException ex) {
            Logger.getLogger(MainWindow.class.getName()).log(Level.SEVERE, null, ex);
        }

    }

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jPanel2 = new javax.swing.JPanel();
        jLabel2 = new javax.swing.JLabel();
        jLabel3 = new javax.swing.JLabel();
        jLabel5 = new javax.swing.JLabel();
        jLabel4 = new javax.swing.JLabel();
        jLabel6 = new javax.swing.JLabel();
        jLabel7 = new javax.swing.JLabel();
        jSeparator2 = new javax.swing.JSeparator();
        jLabel8 = new javax.swing.JLabel();
        jLabel9 = new javax.swing.JLabel();
        jLabel10 = new javax.swing.JLabel();
        jLabel11 = new javax.swing.JLabel();
        jLabel12 = new javax.swing.JLabel();
        jLabel13 = new javax.swing.JLabel();
        jLabel23 = new javax.swing.JLabel();
        jLabel25 = new javax.swing.JLabel();
        jPanel3 = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();
        jTextField1 = new javax.swing.JTextField();
        jSeparator1 = new javax.swing.JSeparator();
        jButton1 = new javax.swing.JButton();
        jButton3 = new javax.swing.JButton();
        jLabel14 = new javax.swing.JLabel();
        jLabel15 = new javax.swing.JLabel();
        jLabel16 = new javax.swing.JLabel();
        jLabel17 = new javax.swing.JLabel();
        jLabel18 = new javax.swing.JLabel();
        jLabel19 = new javax.swing.JLabel();
        jLabel20 = new javax.swing.JLabel();
        jLabel21 = new javax.swing.JLabel();
        jLabel22 = new javax.swing.JLabel();
        jLabel24 = new javax.swing.JLabel();
        customProgressBar1 = new es.uvigo.esei.ephyslab.fortrananalyser.CustomProgressBar();

        setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);
        setBackground(new java.awt.Color(183, 183, 183));
        setCursor(new java.awt.Cursor(java.awt.Cursor.DEFAULT_CURSOR));
        setMinimumSize(new java.awt.Dimension(838, 535));
        setResizable(false);

        jPanel2.setBackground(new java.awt.Color(61, 143, 199));

        jLabel2.setBackground(java.awt.Color.white);
        jLabel2.setFont(new java.awt.Font("Ubuntu", 0, 18)); // NOI18N
        jLabel2.setForeground(java.awt.Color.white);
        jLabel2.setText("version");

        jLabel3.setBackground(java.awt.Color.white);
        jLabel3.setFont(new java.awt.Font("Ubuntu", 0, 18)); // NOI18N
        jLabel3.setForeground(java.awt.Color.white);
        jLabel3.setText("menu1");

        jLabel5.setIcon(new javax.swing.ImageIcon(getClass().getResource("/es/uvigo/esei/ephyslab/fortrananalyser/icon-white_orig.png"))); // NOI18N

        jLabel4.setIcon(new javax.swing.ImageIcon(getClass().getResource("/es/uvigo/esei/ephyslab/fortrananalyser/aboutIcon.png"))); // NOI18N

        jLabel6.setBackground(java.awt.Color.white);
        jLabel6.setFont(new java.awt.Font("Ubuntu", 0, 18)); // NOI18N
        jLabel6.setForeground(java.awt.Color.white);
        jLabel6.setText("menu2");

        jLabel7.setFont(new java.awt.Font("Ubuntu", 0, 24)); // NOI18N
        jLabel7.setForeground(java.awt.Color.white);
        jLabel7.setText("FortranAnalyser");

        jLabel8.setBackground(java.awt.Color.white);
        jLabel8.setForeground(java.awt.Color.white);
        jLabel8.setText("jLabel8");
        jLabel8.setName(""); // NOI18N

        jLabel9.setBackground(java.awt.Color.white);
        jLabel9.setForeground(java.awt.Color.white);
        jLabel9.setText("jLabel9");

        jLabel10.setBackground(java.awt.Color.white);
        jLabel10.setForeground(java.awt.Color.white);
        jLabel10.setText("jLabel10");

        jLabel11.setBackground(java.awt.Color.white);
        jLabel11.setForeground(java.awt.Color.white);
        jLabel11.setText("jLabel11");

        jLabel12.setBackground(java.awt.Color.white);
        jLabel12.setForeground(java.awt.Color.white);
        jLabel12.setText("jLabel12");

        jLabel13.setBackground(java.awt.Color.white);
        jLabel13.setForeground(java.awt.Color.white);
        jLabel13.setText("jLabel13");

        jLabel23.setFont(new java.awt.Font("Ubuntu", 0, 11)); // NOI18N
        jLabel23.setForeground(java.awt.Color.white);
        jLabel23.setText("www.fortrananalyser.ephyslab.uvigo.es");
        jLabel23.setToolTipText("");

        jLabel25.setBackground(java.awt.Color.white);
        jLabel25.setForeground(java.awt.Color.white);
        jLabel25.setText("jLabel25");

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addGap(18, 18, 18)
                        .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(jPanel2Layout.createSequentialGroup()
                                .addComponent(jLabel5)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                .addComponent(jLabel3, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                            .addGroup(jPanel2Layout.createSequentialGroup()
                                .addComponent(jLabel4)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                .addComponent(jLabel6, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))))
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel2Layout.createSequentialGroup()
                        .addContainerGap()
                        .addComponent(jSeparator2))
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel2Layout.createSequentialGroup()
                        .addContainerGap(23, Short.MAX_VALUE)
                        .addComponent(jLabel7)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addComponent(jLabel2)
                        .addGap(15, 15, 15))
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(jPanel2Layout.createSequentialGroup()
                                .addGap(44, 44, 44)
                                .addComponent(jLabel23, javax.swing.GroupLayout.PREFERRED_SIZE, 203, javax.swing.GroupLayout.PREFERRED_SIZE))
                            .addGroup(jPanel2Layout.createSequentialGroup()
                                .addGap(76, 76, 76)
                                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                                    .addComponent(jLabel10, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                    .addComponent(jLabel11, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                    .addComponent(jLabel9, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                    .addComponent(jLabel8, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                    .addComponent(jLabel25, javax.swing.GroupLayout.PREFERRED_SIZE, 105, javax.swing.GroupLayout.PREFERRED_SIZE))))
                        .addGap(0, 0, Short.MAX_VALUE)))
                .addContainerGap())
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel2Layout.createSequentialGroup()
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jLabel13, javax.swing.GroupLayout.PREFERRED_SIZE, 160, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel12, javax.swing.GroupLayout.PREFERRED_SIZE, 160, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addGap(62, 62, 62))
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel2Layout.createSequentialGroup()
                .addGap(24, 24, 24)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel7)
                    .addComponent(jLabel2))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jSeparator2, javax.swing.GroupLayout.PREFERRED_SIZE, 10, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(32, 32, 32)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jLabel3)
                    .addComponent(jLabel5))
                .addGap(18, 18, 18)
                .addComponent(jLabel8, javax.swing.GroupLayout.PREFERRED_SIZE, 23, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jLabel9, javax.swing.GroupLayout.PREFERRED_SIZE, 23, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jLabel10, javax.swing.GroupLayout.PREFERRED_SIZE, 23, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jLabel11, javax.swing.GroupLayout.PREFERRED_SIZE, 25, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jLabel25, javax.swing.GroupLayout.PREFERRED_SIZE, 25, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(50, 50, 50)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jLabel4, javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jLabel6, javax.swing.GroupLayout.Alignment.TRAILING))
                .addGap(18, 18, 18)
                .addComponent(jLabel12, javax.swing.GroupLayout.PREFERRED_SIZE, 23, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(18, 18, 18)
                .addComponent(jLabel13, javax.swing.GroupLayout.PREFERRED_SIZE, 26, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addComponent(jLabel23)
                .addContainerGap())
        );

        jLabel2.getAccessibleContext().setAccessibleDescription("");

        jPanel3.setBackground(new java.awt.Color(234, 234, 234));
        jPanel3.setBorder(null);

        jLabel1.setIcon(new javax.swing.ImageIcon(getClass().getResource("/es/uvigo/esei/ephyslab/fortrananalyser/headerTitle.png"))); // NOI18N
        jLabel1.setToolTipText("");

        jTextField1.setEditable(false);
        jTextField1.setBackground(new java.awt.Color(234, 234, 234));
        jTextField1.setFont(new java.awt.Font("Ubuntu", 2, 15)); // NOI18N
        jTextField1.setForeground(new java.awt.Color(15, 15, 15));
        jTextField1.setText("jTextField1");
        jTextField1.setToolTipText("");
        jTextField1.setBorder(null);

        jSeparator1.setBackground(new java.awt.Color(76, 76, 76));
        jSeparator1.setToolTipText("");
        jSeparator1.setBorder(null);

        jButton1.setBackground(new java.awt.Color(212, 212, 212));
        jButton1.setForeground(java.awt.Color.black);
        jButton1.setText(". . .");
        jButton1.setBorder(null);
        jButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton1ActionPerformed(evt);
            }
        });

        jButton3.setBackground(new java.awt.Color(255, 110, 25));
        jButton3.setForeground(java.awt.Color.white);
        jButton3.setText("jButton3");
        jButton3.setBorder(null);
        jButton3.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton3ActionPerformed(evt);
            }
        });

        jLabel14.setBackground(new java.awt.Color(234, 234, 234));
        jLabel14.setForeground(new java.awt.Color(128, 19, 25));
        jLabel14.setText("Error message");

        jLabel15.setFont(new java.awt.Font("Ubuntu", 1, 16)); // NOI18N

        jLabel16.setFont(new java.awt.Font("Ubuntu", 1, 14)); // NOI18N
        jLabel16.setText("jLabel16");

        jLabel17.setFont(new java.awt.Font("Ubuntu", 1, 14)); // NOI18N
        jLabel17.setText("jLabel17");

        jLabel18.setFont(new java.awt.Font("Ubuntu", 1, 14)); // NOI18N
        jLabel18.setText("jLabel18");

        jLabel19.setFont(new java.awt.Font("Ubuntu", 0, 14)); // NOI18N
        jLabel19.setText("jLabel19");

        jLabel20.setFont(new java.awt.Font("Ubuntu", 0, 14)); // NOI18N
        jLabel20.setText("jLabel20");

        jLabel21.setFont(new java.awt.Font("Ubuntu", 0, 14)); // NOI18N
        jLabel21.setText("jLabel21");

        jLabel22.setIcon(new javax.swing.ImageIcon(getClass().getResource("/es/uvigo/esei/ephyslab/fortrananalyser/pdf-50.png"))); // NOI18N
        jLabel22.setToolTipText("");
        jLabel22.setMaximumSize(new java.awt.Dimension(57, 50));
        jLabel22.setMinimumSize(new java.awt.Dimension(57, 50));
        jLabel22.setPreferredSize(new java.awt.Dimension(50, 50));

        jLabel24.setFont(new java.awt.Font("Ubuntu", 0, 10)); // NOI18N
        jLabel24.setText("jLabel24");

        customProgressBar1.setBackground(new java.awt.Color(234, 234, 234));

        javax.swing.GroupLayout customProgressBar1Layout = new javax.swing.GroupLayout(customProgressBar1);
        customProgressBar1.setLayout(customProgressBar1Layout);
        customProgressBar1Layout.setHorizontalGroup(
            customProgressBar1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 143, Short.MAX_VALUE)
        );
        customProgressBar1Layout.setVerticalGroup(
            customProgressBar1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 125, Short.MAX_VALUE)
        );

        javax.swing.GroupLayout jPanel3Layout = new javax.swing.GroupLayout(jPanel3);
        jPanel3.setLayout(jPanel3Layout);
        jPanel3Layout.setHorizontalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addGap(63, 63, 63)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel3Layout.createSequentialGroup()
                        .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING, false)
                            .addGroup(javax.swing.GroupLayout.Alignment.LEADING, jPanel3Layout.createSequentialGroup()
                                .addComponent(jLabel18, javax.swing.GroupLayout.PREFERRED_SIZE, 86, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jLabel21, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                            .addGroup(jPanel3Layout.createSequentialGroup()
                                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                    .addGroup(jPanel3Layout.createSequentialGroup()
                                        .addComponent(jLabel16, javax.swing.GroupLayout.PREFERRED_SIZE, 86, javax.swing.GroupLayout.PREFERRED_SIZE)
                                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                        .addComponent(jLabel19, javax.swing.GroupLayout.DEFAULT_SIZE, 209, Short.MAX_VALUE))
                                    .addGroup(jPanel3Layout.createSequentialGroup()
                                        .addComponent(jLabel17, javax.swing.GroupLayout.PREFERRED_SIZE, 86, javax.swing.GroupLayout.PREFERRED_SIZE)
                                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                        .addComponent(jLabel20, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                                    .addComponent(jLabel15, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                                .addGap(112, 112, 112)))
                        .addGap(0, 0, Short.MAX_VALUE))
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel3Layout.createSequentialGroup()
                        .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING, false)
                            .addComponent(jSeparator1, javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jTextField1, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, 413, Short.MAX_VALUE)
                            .addComponent(jLabel14, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.PREFERRED_SIZE, 403, javax.swing.GroupLayout.PREFERRED_SIZE))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButton1, javax.swing.GroupLayout.DEFAULT_SIZE, 44, Short.MAX_VALUE)
                        .addGap(25, 25, 25))))
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel3Layout.createSequentialGroup()
                .addGap(0, 0, Short.MAX_VALUE)
                .addComponent(jLabel1, javax.swing.GroupLayout.PREFERRED_SIZE, 280, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(155, 155, 155))
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel3Layout.createSequentialGroup()
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel3Layout.createSequentialGroup()
                        .addComponent(jLabel22, javax.swing.GroupLayout.PREFERRED_SIZE, 57, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addGap(46, 46, 46))
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel3Layout.createSequentialGroup()
                        .addComponent(jLabel24, javax.swing.GroupLayout.PREFERRED_SIZE, 86, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addContainerGap())))
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel3Layout.createSequentialGroup()
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addComponent(customProgressBar1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(0, 0, Short.MAX_VALUE))
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel3Layout.createSequentialGroup()
                .addGap(0, 0, Short.MAX_VALUE)
                .addComponent(jButton3, javax.swing.GroupLayout.PREFERRED_SIZE, 120, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(210, 210, 210))
        );
        jPanel3Layout.setVerticalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jLabel1, javax.swing.GroupLayout.PREFERRED_SIZE, 89, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(28, 28, 28)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jTextField1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jButton1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jSeparator1, javax.swing.GroupLayout.PREFERRED_SIZE, 12, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jLabel14)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jButton3, javax.swing.GroupLayout.PREFERRED_SIZE, 42, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(18, 18, 18)
                .addComponent(customProgressBar1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jLabel15, javax.swing.GroupLayout.PREFERRED_SIZE, 30, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(18, 18, 18)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jLabel16)
                    .addComponent(jLabel19))
                .addGap(18, 18, 18)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel17)
                    .addComponent(jLabel20))
                .addGap(18, 18, 18)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel18)
                    .addComponent(jLabel21))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jLabel22, javax.swing.GroupLayout.PREFERRED_SIZE, 50, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jLabel24, javax.swing.GroupLayout.PREFERRED_SIZE, 17, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(20, 20, 20))
        );

        jTextField1.getAccessibleContext().setAccessibleName("panelText1");
        jButton1.getAccessibleContext().setAccessibleName("fileChoser");
        jButton1.getAccessibleContext().setAccessibleDescription("");

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addComponent(jPanel2, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addGap(1, 1, 1)
                .addComponent(jPanel3, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jPanel2, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
            .addComponent(jPanel3, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents

    /**
     * Button to analyse the selected directory. Show message error in case that
     * the user not chose a directory
     *
     * @param evt push the button event
     */
    private void jButton3ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton3ActionPerformed

        if (this.jTextField1.getText().equals(this.messages.getString("selectDirectory"))) {
            this.jLabel14.setVisible(true);
        } else {
            this.jLabel14.setVisible(false);
            this.customProgressBar1.setVisible(true);
            this.customProgressBar1.updateProgressBar(0);

            executeAnalyse(this.jTextField1.getText());

        }
    }//GEN-LAST:event_jButton3ActionPerformed

    /**
     * Action event for button "..." to select the directory path.
     *
     * @param evt the event to push the button
     */
    private void jButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton1ActionPerformed

        if (this.fc.showOpenDialog(MainWindow.this) == JFileChooser.APPROVE_OPTION) {

            File file = this.fc.getSelectedFile();
            this.jTextField1.setText(file.getAbsolutePath());
        }
    }//GEN-LAST:event_jButton1ActionPerformed

    /**
     * call the execution of the analisis in a new Thread
     *
     * @param pathFile the path of the file
     */
    private void executeAnalyse(String pathFile) {

        hideComponents();

        TasksBar t = new TasksBar(MainWindow.this, pathFile, MainWindow.this.messages);
        this.setEnabled(false);
        this.jButton1.setEnabled(false);
        this.jButton3.setEnabled(false);
        t.execute();

    }

    /**
     * hide all label components
     */
    private void hideComponents() {
        this.jLabel15.setVisible(false);
        this.jLabel16.setVisible(false);
        this.jLabel17.setVisible(false);
        this.jLabel18.setVisible(false);
        this.jLabel19.setVisible(false);
        this.jLabel20.setVisible(false);
        this.jLabel21.setVisible(false);
        this.jLabel22.setVisible(false);
        this.jLabel24.setVisible(false);
    }

    /**
     * This method initialize all variables on this class.
     */
    private void initializeVariables() {

        this.setLocationRelativeTo(null);
        this.setTitle("FortranAnalyser");
        this.setIconImage(new ImageIcon(MainWindow.class.getResource("fortranAnalyserIcon.png")).getImage());
        this.currentLocale = new Locale(MainWindow.DEFAULT_LANGUAGE, MainWindow.DEFAULT_COUNTRY);
        Locale.setDefault(currentLocale);
        this.messages = ResourceBundle.getBundle(MainWindow.BUNDLE, currentLocale);
        this.jLabel14.setVisible(false);
        this.customProgressBar1.setVisible(false);
        hideComponents();

        /**
         * Configure buttons and text buttons
         */
        this.jTextField1.setText(this.messages.getString("selectDirectory"));
        this.jButton3.setText(this.messages.getString("nameButtonAnalyse"));
        this.jLabel2.setText(this.messages.getString("version"));
        this.jLabel3.setText(this.messages.getString("nameMenu"));
        this.jLabel6.setText(this.messages.getString("about"));
        this.jLabel8.setText(this.messages.getString("nameButtonEnglish"));
        this.jLabel9.setText(this.messages.getString("nameButtonFrench"));
        this.jLabel10.setText(this.messages.getString("nameButtonSpanish"));
        this.jLabel11.setText(this.messages.getString("nameButtonGalician"));
        this.jLabel25.setText(this.messages.getString("nameButtonPortuges"));
        this.jLabel12.setText(this.messages.getString("manual"));
        this.jLabel13.setText(this.messages.getString("license"));
        this.jLabel14.setText(this.messages.getString("errorDirectoryEmpty"));
        this.jLabel15.setText(this.messages.getString("analisisResults"));
        this.jLabel16.setText(this.messages.getString("timeResults"));
        this.jLabel17.setText(this.messages.getString("arithmeticAverage"));
        this.jLabel18.setText(this.messages.getString("directoryMessage"));
        this.jLabel24.setText(this.messages.getString("openFile"));

        this.fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

    }

    /**
     * Components to use in case the programme is lanched by console
     */
    private void initialiceComponentsNoGUI() {

        this.currentLocale = new Locale(MainWindow.DEFAULT_LANGUAGE, MainWindow.DEFAULT_COUNTRY);
        Locale.setDefault(currentLocale);
        this.messages = ResourceBundle.getBundle(MainWindow.BUNDLE, currentLocale);

    }

    public JLabel getjLabel15() {
        return jLabel15;
    }

    public JLabel getjLabel16() {
        return jLabel16;
    }

    public JLabel getjLabel17() {
        return jLabel17;
    }

    public JLabel getjLabel18() {
        return jLabel18;
    }

    public JLabel getjLabel19() {
        return jLabel19;
    }

    public JLabel getjLabel20() {
        return jLabel20;
    }

    public JLabel getjLabel21() {
        return jLabel21;
    }

    public JLabel getjLabel22() {
        return jLabel22;
    }

    public JLabel getjLabel24() {
        return jLabel24;
    }

    public JButton getjButton1() {
        return jButton1;
    }

    public JButton getjButton3() {
        return jButton3;
    }


    // Variables declaration - do not modify//GEN-BEGIN:variables
    public es.uvigo.esei.ephyslab.fortrananalyser.CustomProgressBar customProgressBar1;
    private javax.swing.JButton jButton1;
    private javax.swing.JButton jButton3;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel10;
    private javax.swing.JLabel jLabel11;
    private javax.swing.JLabel jLabel12;
    private javax.swing.JLabel jLabel13;
    private javax.swing.JLabel jLabel14;
    private javax.swing.JLabel jLabel15;
    private javax.swing.JLabel jLabel16;
    private javax.swing.JLabel jLabel17;
    private javax.swing.JLabel jLabel18;
    private javax.swing.JLabel jLabel19;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel20;
    private javax.swing.JLabel jLabel21;
    private javax.swing.JLabel jLabel22;
    private javax.swing.JLabel jLabel23;
    private javax.swing.JLabel jLabel24;
    private javax.swing.JLabel jLabel25;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JLabel jLabel7;
    private javax.swing.JLabel jLabel8;
    private javax.swing.JLabel jLabel9;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JSeparator jSeparator1;
    private javax.swing.JSeparator jSeparator2;
    private javax.swing.JTextField jTextField1;
    // End of variables declaration//GEN-END:variables
}
