/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.gui5;

import java.awt.BorderLayout;
import java.io.IOException;
import javax.swing.JFrame;
import org.slf4j.LoggerFactory;

import uika806.Services;
import uika806.port.CurrentPort;
import uika806.small.env.EnvironFactory;

/**
 *
 */
public class RunV5 extends JFrame  {
    
    private static final org.slf4j.Logger LOG = LoggerFactory.getLogger(RunV5.class);

    JPanelV5 panel = new JPanelV5();
    
    public RunV5() {
        
        setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);
        this.setSize(980, 560);
        setTitle("uika806 (tiny r7rs scheme)");
        this.getContentPane().add(panel, BorderLayout.CENTER);
    }
    
    public static RunV5 theApp;
    
    public static void main(String args[]) {

        try {
            for (javax.swing.UIManager.LookAndFeelInfo info : javax.swing.UIManager.getInstalledLookAndFeels()) {
                if ("Nimbus".equals(info.getName())) {
                    javax.swing.UIManager.setLookAndFeel(info.getClassName());
                    break;
                }
            }
        } catch (Exception ex) {
            System.err.println("Exception : " + ex.getMessage());
            return;
        }

        String property = System.getProperty("java.version");
        LOG.info("java.version={}", property);
        String property2 = System.getProperty("java.vendor");
        LOG.info("java.vendor={}", property2);


        try {
            CurrentPort.init(true);
        } catch (IOException ioe) {
            LOG.error("IOException", ioe);
            return;
        }

        theApp = new RunV5();
        EnvironFactory factory = new EnvironFactory();
        factory.loadBase("startup5.scm", 5);

        theApp.panel.lexEnv = factory.getFirstEnviron(true, false);

        Services.environFactory = factory;
        
        LOG.info("_____________ Initialize done _____________");
        
        final RunV5 ef = theApp;
        
        /* Create and display the form */
        java.awt.EventQueue.invokeLater(new Runnable() {
            @Override
            public void run() {
                ef.setVisible(true);
            }
        });
        
    }
    
}
