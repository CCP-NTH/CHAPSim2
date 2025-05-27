"""
README
-------
This script performs post-processing and visualization of channel flow simulation data from CHAPSim2.

Features:
- Plots mean velocity profiles (ux, uy, uz)
- Plots pressure profiles
- Plots Reynolds stress components (uu, vv, ww, uv)
- Generates combined RMS velocity plots
- Compares results with MKM180 reference data

Requirements:
- Python 3.x
- NumPy
- Matplotlib

Usage:
1. Ensure the following data structure:
   - ../1_data/: Contains CHAPSim2 simulation results
   - ../../MKM180_profiles/: Contains reference data files (chan180.means, chan180.reystress)

2. Configure the DNS_TIME variable in the code to match your simulation time step

3. Run the script:
   python channel_postprocess_plot.py

Output:
- Individual component plots: channel_[component].png
- Combined RMS plot: channel_all_rms.png

Author: W Wang (STFC)
"""
# import necessary libraries
import os
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.cm as cm
import math
from pylab import rcParams

# User configuration
DNS_TIME = "200000"  # Time value for DNS data analysis

# Define colormaps and markers
cbrg = cm.get_cmap(name='brg', lut=None)
cgry = cm.get_cmap(name='gray', lut=None)
cbow = cm.get_cmap(name='gist_rainbow', lut=None)
mlst = ["o", "<", "*", "v", "^", '>', '1', '2', '3', '4', 'x', 's', '8', '+']

# Set plot configurations
plt.rc('figure', facecolor="white")
plt.rc('legend', fontsize=15)
rcParams['legend.loc'] = 'best'

# Plot settings
figsize = (9, 6)
dpi = 500

# Parameters configurations
PARAMS = {
    'ux': {
        'ref_file': 'chan180.means',
        'ref_cols': ['y', 'yplus', 'umean', 'umeandy', 'wmean', 'wmeandy', 'pmean'],
        'ref_skiprows': 25,
        'ylabel': r'$u_x^+$',
        'ref_key': 'umean',
        'scaling': 'mean'
    },
    'uy': {
        'ref_file': 'chan180.means',
        'ref_cols': ['y', 'yplus', 'umean', 'umeandy', 'wmean', 'wmeandy', 'pmean'],
        'ref_skiprows': 25,
        'ylabel': r'$u_y^+$',
        'ref_key': 'umean',  # This won't be used as we'll plot zero
        'scaling': 'mean'
    },
    'uz': {
        'ref_file': 'chan180.means',
        'ref_cols': ['y', 'yplus', 'umean', 'umeandy', 'wmean', 'wmeandy', 'pmean'],
        'ref_skiprows': 25,
        'ylabel': r'$u_z^+$',
        'ref_key': 'wmean',
        'scaling': 'mean'
    },
    'pr': {
        'ref_file': 'chan180.means',
        'ref_cols': ['y', 'yplus', 'umean', 'umeandy', 'wmean', 'wmeandy', 'pmean'],
        'ref_skiprows': 25,
        'ylabel': r'$p^+$',
        'ref_key': 'pmean',
        'scaling': 'mean'
    },
    'uu': {
        'ref_file': 'chan180.reystress',
        'ref_cols': ['y', 'yplus', 'uu', 'vv', 'ww', 'uv', 'uw', 'vw'],
        'ref_skiprows': 25,
        'ylabel': r'$u_{rms}^+$',
        'ref_key': 'uu',
        'scaling': 'rms'
    },
    'vv': {
        'ref_file': 'chan180.reystress',
        'ref_cols': ['y', 'yplus', 'uu', 'vv', 'ww', 'uv', 'uw', 'vw'],
        'ref_skiprows': 25,
        'ylabel': r'$v_{rms}^+$',
        'ref_key': 'vv',
        'scaling': 'rms'
    },
    'ww': {
        'ref_file': 'chan180.reystress',
        'ref_cols': ['y', 'yplus', 'uu', 'vv', 'ww', 'uv', 'uw', 'vw'],
        'ref_skiprows': 25,
        'ylabel': r'$w_{rms}^+$',
        'ref_key': 'ww',
        'scaling': 'rms'
    },
    'uv': {
        'ref_file': 'chan180.reystress',
        'ref_cols': ['y', 'yplus', 'uu', 'vv', 'ww', 'uv', 'uw', 'vw'],
        'ref_skiprows': 25,
        'ylabel': r'$\overline{u^\prime v^\prime}^+$',
        'ref_key': 'uv',
        'scaling': 'reynolds'
    }
}

class ChannelFlowAnalyzer:
    def __init__(self, param_name):
        if param_name not in PARAMS:
            raise ValueError(f"Unknown parameter: {param_name}")
        
        self.param = PARAMS[param_name]
        self.param_name = param_name
        self.setup_constants()
        self.load_reference_data()
        self.load_flow_data()
        
    def setup_constants(self):
        """Initialize constants for the analysis"""
        self.Re = 2800
        self.Re0 = 2784.0
        self.Ret = 178.12
        self.utau0 = self.Ret / self.Re0
        
        # Print reference values
        print(f"\nFor reference data:")
        print(f"utau0 = {self.utau0:.6f}")
        print(f"tauw0 = {self.utau0 * self.utau0:.6f}")
        
        # Calculate wall quantities from ux data
        ux_file = os.path.join('../1_data', f"domain1_time_space_averaged_ux_{DNS_TIME}.dat")
        ux_data = np.genfromtxt(ux_file, names=['j', 'y', 'ux'])
        dudy = ux_data['ux'][0] / (1.0 + ux_data['y'][0])
        self.tauw = dudy / self.Re
        self.utau = math.sqrt(abs(self.tauw))
        
        # Print calculated values
        print(f"\nFor all parameters (calculated from ux):")
        print(f"u_tau = {self.utau:.6f}")
        print(f"tau_w = {self.tauw:.6f}\n")
        
    def load_reference_data(self):
        """Load reference data from MKM180 profiles"""
        ref_path = '../../MKM180_profiles'
        ref_file = os.path.join(ref_path, self.param['ref_file'])
        self.ref_data = np.genfromtxt(
            ref_file, 
            delimiter=None, 
            skip_header=self.param['ref_skiprows'], 
            skip_footer=0,
            names=self.param['ref_cols']
        )
    
    def load_flow_data(self):
        """Load and process flow data"""
        dns_path = '../1_data'
        dns_file = os.path.join(dns_path, f"domain1_time_space_averaged_{self.param_name}_{DNS_TIME}.dat")
        data = np.genfromtxt(
            dns_file, 
            delimiter=None, 
            skip_header=0, 
            skip_footer=0,
            names=['j', 'y', self.param_name]
        )
        # Convert structured array to dictionary for easier manipulation
        self.dns_data = {name: data[name] for name in data.dtype.names}
        
        # Transform y coordinates to match reference data
        y = self.dns_data['y']
        y_transformed = np.zeros_like(y)
        
        # Apply transformation: y=y+1 for -1<y<0 and y=1-y for 0<y<1
        for i in range(len(y)):
            if -1 < y[i] < 0:
                y_transformed[i] = y[i] + 1
            elif 0 <= y[i] < 1:
                y_transformed[i] = 1 - y[i]
                
        self.dns_data['y'] = y_transformed
        
        # Process the data
        self.process_data()
        
    def process_data(self):
        """Process the data based on parameter type"""
        # Calculate y+
        self.dns_data['yplus'] = self.Re * self.utau * self.dns_data['y']
        
        if self.param_name == 'pr':
            # For pressure, scale by tauw and shift to make first point zero
            pr_data = self.dns_data[self.param_name] / self.tauw
            pr_data = pr_data - pr_data[0]  # Shift data to make first point zero
            self.dns_data['scaled'] = pr_data
        elif self.param['scaling'] == 'mean':
            # Process mean velocity
            self.dns_data['scaled'] = self.dns_data[self.param_name] / self.utau
        elif self.param['scaling'] == 'rms':
            # Load mean velocities for RMS calculations
            ux_file = os.path.join('../1_data', f"domain1_time_space_averaged_ux_{DNS_TIME}.dat")
            uz_file = os.path.join('../1_data', f"domain1_time_space_averaged_uz_{DNS_TIME}.dat")
            uy = np.zeros_like(self.dns_data['y'])  # uy is zero in channel flow
            
            # Load and transform ux data
            ux_data = np.genfromtxt(ux_file, names=['j', 'y', 'ux'])
            y = ux_data['y']
            y_transformed = np.zeros_like(y)
            for i in range(len(y)):
                if -1 < y[i] < 0:
                    y_transformed[i] = y[i] + 1
                elif 0 <= y[i] < 1:
                    y_transformed[i] = 1 - y[i]
            ux = ux_data['ux']
            
            # Load and transform uz data
            uz_data = np.genfromtxt(uz_file, names=['j', 'y', 'uz'])
            uz = uz_data['uz']
            
            # Calculate RMS values based on parameter
            if self.param_name == 'uu':
                self.dns_data['scaled'] = np.sqrt(self.dns_data[self.param_name] - ux * ux) / self.utau
            elif self.param_name == 'vv':
                self.dns_data['scaled'] = np.sqrt(self.dns_data[self.param_name] - uy * uy) / self.utau
            elif self.param_name == 'ww':
                self.dns_data['scaled'] = np.sqrt(self.dns_data[self.param_name] - uz * uz) / self.utau
                
        elif self.param['scaling'] == 'reynolds':
            # For uv correlation
            ux_file = os.path.join('../1_data', f"domain1_time_space_averaged_ux_{DNS_TIME}.dat")
            ux_data = np.genfromtxt(ux_file, names=['j', 'y', 'ux'])
            uy = np.zeros_like(self.dns_data['y'])  # uy is zero in channel flow
            
            # Transform coordinates for ux
            y = ux_data['y']
            y_transformed = np.zeros_like(y)
            for i in range(len(y)):
                if -1 < y[i] < 0:
                    y_transformed[i] = y[i] + 1
                elif 0 <= y[i] < 1:
                    y_transformed[i] = 1 - y[i]
            ux = ux_data['ux']
            
            # Calculate Reynolds stress
            self.dns_data['scaled'] = (self.dns_data[self.param_name] - ux * uy) / (self.utau * self.utau)
            
    def plot_profile(self):
        """Plot the flow profile"""
        fig, ax = plt.subplots(figsize=figsize, dpi=dpi)
        
        # Set labels
        ax.set_xlabel(r'$y^+$', fontsize=20)
        ax.set_ylabel(self.param['ylabel'], fontsize=20)
        
        # Special handling for uy (reference is zero)
        if self.param_name == 'uy':
            # Generate zero reference line
            yplus_ref = np.logspace(-1, 3, 100)  # Create log-spaced points
            zeros_ref = np.zeros_like(yplus_ref)
            
            # Plot reference data
            ax.plot(
                yplus_ref,
                zeros_ref,
                marker=mlst[1],
                mfc='none',
                ms=4,
                mec=cbrg(0.00),
                color=cbrg(0.00),
                linestyle='-',
                label='MKM180'
            )
        else:
            # Prepare reference data - take sqrt for Reynolds stress terms
            ref_data_plot = self.ref_data[self.param['ref_key']]
            if self.param['scaling'] == 'rms':
                ref_data_plot = np.sqrt(ref_data_plot)
            elif self.param['scaling'] == 'reynolds':
                ref_data_plot = np.abs(ref_data_plot)
                mask = self.ref_data['yplus'] > 0
                ref_data_plot = ref_data_plot[mask]
                ref_yplus = self.ref_data['yplus'][mask]
            else:
                ref_yplus = self.ref_data['yplus']
            
            # Plot reference data
            ax.plot(
                ref_yplus if self.param['scaling'] == 'reynolds' else self.ref_data['yplus'],
                ref_data_plot,
                marker=mlst[1],
                mfc='none',
                ms=4,
                mec=cbrg(0.00),
                color=cbrg(0.00),
                linestyle='None',
                label='MKM180'
            )
        
        # Plot DNS data
        if self.param['scaling'] == 'reynolds':
            self.dns_data['scaled'] = np.abs(self.dns_data['scaled'])
            mask = self.dns_data['yplus'] > 0
            plot_yplus = self.dns_data['yplus'][mask]
            plot_scaled = self.dns_data['scaled'][mask]
        else:
            plot_yplus = self.dns_data['yplus']
            plot_scaled = self.dns_data['scaled']
        
        ax.plot(
            plot_yplus,
            plot_scaled,
            marker='none',
            color=cbrg(0.50),
            linestyle='-.',
            label='CHAPsim2'
        )
        
        # Set x-axis to log scale and range
        ax.set_xscale('log')
        ax.set_xlim(0.1, 500)
        
        # Customize plot
        ax.legend(loc='upper left', ncol=1, labelspacing=0.1, frameon=False, handlelength=3.2, numpoints=1)
        ax.grid(True, which="both", ls="-", alpha=0.2)
        
        # Save plot
        fig.savefig(f'channel_{self.param_name}.png')
        print(f"channel_{self.param_name}... Done!")
        plt.close('all')

    def plot_all_rms(self):
        """Plot all RMS components in one figure"""
        fig, ax = plt.subplots(figsize=figsize, dpi=dpi)
        
        # Set labels
        ax.set_xlabel(r'$y^+$', fontsize=20)
        ax.set_ylabel(r'$u_{i,rms}^+$', fontsize=20)
        
        # Colors and markers for different components
        colors = {'uu': cbrg(0.0), 'vv': cbrg(0.3), 'ww': cbrg(0.6)}
        markers = {'uu': 'o', 'vv': 's', 'ww': '^'}  # Different markers for each component
        labels = {'uu': r'$u_{rms}^+$', 'vv': r'$v_{rms}^+$', 'ww': r'$w_{rms}^+$'}
        
        # Load reference data for all components
        ref_path = '../../MKM180_profiles'
        ref_file = os.path.join(ref_path, 'chan180.reystress')
        ref_data = np.genfromtxt(
            ref_file,
            delimiter=None,
            skip_header=25,
            skip_footer=0,
            names=['y', 'yplus', 'uu', 'vv', 'ww', 'uv', 'uw', 'vw']
        )
        
        # Plot reference data for each component
        for comp in ['uu', 'vv', 'ww']:
            ax.plot(
                ref_data['yplus'],
                np.sqrt(ref_data[comp]),
                marker=markers[comp],
                mfc='none',
                ms=6,
                mec=colors[comp],
                color=colors[comp],
                linestyle='None',
                markevery=1,  # Adjust marker frequency
                label=f'MKM180 {labels[comp]}'
            )
        
        # Load and process DNS data for each component
        for comp in ['uu', 'vv', 'ww']:
            # Load the data
            dns_file = os.path.join('../1_data', f"domain1_time_space_averaged_{comp}_{DNS_TIME}.dat")
            data = np.genfromtxt(dns_file, names=['j', 'y', comp])
            
            # Transform y coordinates
            y = data['y']
            y_transformed = np.zeros_like(y)
            for i in range(len(y)):
                if -1 < y[i] < 0:
                    y_transformed[i] = y[i] + 1
                elif 0 <= y[i] < 1:
                    y_transformed[i] = 1 - y[i]
            
            # Calculate yplus
            yplus = self.Re * self.utau * y_transformed
            
            # Load mean velocities if needed
            if comp == 'uu':
                ux_file = os.path.join('../1_data', f"domain1_time_space_averaged_ux_{DNS_TIME}.dat")
                ux_data = np.genfromtxt(ux_file, names=['j', 'y', 'ux'])
                mean_vel = ux_data['ux']
            elif comp == 'vv':
                mean_vel = np.zeros_like(y)  # uy is zero in channel flow
            else:  # ww
                uz_file = os.path.join('../1_data', f"domain1_time_space_averaged_uz_{DNS_TIME}.dat")
                uz_data = np.genfromtxt(uz_file, names=['j', 'y', 'uz'])
                mean_vel = uz_data['uz']
            
            # Calculate RMS
            rms = np.sqrt(data[comp] - mean_vel * mean_vel) / self.utau
            
            # Plot DNS data
            ax.plot(
                yplus,
                rms,
                color=colors[comp],
                linestyle='-.',
                label=f'CHAPsim2 {labels[comp]}'
            )
        
        # Set x-axis to log scale and range
        ax.set_xscale('log')
        ax.set_xlim(0.1, 500)
        
        # Customize plot
        ax.legend(loc='upper left', ncol=1, labelspacing=0.1, frameon=False, handlelength=3.2, numpoints=1)
        ax.grid(True, which="both", ls="-", alpha=0.2)
        
        # Save plot
        fig.savefig('channel_all_rms.png')
        print("channel_all_rms... Done!")
        plt.close('all')

def main():
    # Initialize with any parameter to get the common values (Re, utau, etc.)
    analyzer = ChannelFlowAnalyzer('ux')
    
    # Plot individual components
    for param_name in ['ux', 'uy', 'uz', 'pr', 'uu', 'vv', 'ww', 'uv']:
        analyzer = ChannelFlowAnalyzer(param_name)
        analyzer.plot_profile()
    
    # Plot all RMS components together
    analyzer.plot_all_rms()

if __name__ == "__main__":
    main()












