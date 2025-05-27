"""
PIPE FLOW POST-PROCESSING AND VISUALIZATION
===========================================

This script performs post-processing and visualization of pipe flow simulation data from CHAPSim2,
comparing the results with reference DNS data.

Features:
- Plots mean velocity profiles (ux, uy, uz)
- Plots pressure profiles
- Plots Reynolds stress components (uu, vv, ww, uv)
- Generates combined RMS velocity plots
- Compares results with reference DNS data
- Reference data: https://dataverse.tdl.org/dataset.xhtml;jsessionid=4175f84626ca494e3b9a96b18d83?persistentId=doi%3A10.18738%2FT8%2FHLC3QY&version=&q=&fileTypeGroupFacet=&fileAccess=Public&fileSortField=date&tagPresort=false

Requirements:
- Python 3.x
- NumPy
- Matplotlib

Usage:
1. Set the configuration parameters in the USER CONFIGURATION section
2. Run the script: python pipe_postprocess_plot.py

Output:
- Individual component plots: pipe_[component].png
- Combined RMS plot: pipe_all_rms.png

Author: W Wang (STFC)
"""
import os
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.cm as cm
import math
import argparse
from pathlib import Path

#==============================================================================
# USER CONFIGURATION
#==============================================================================

# Default configurations (can be overridden via command-line arguments)
DEFAULT_CONFIG = {
    'dns_time': "320000",     # Time value for DNS data analysis
    'data_dir': '../1_data',  # Directory containing CHAPSim2 simulation results
    'ref_dir': '../../refdata/dataverse_files', # Directory containing reference data files
    'output_dir': './',       # Directory for output plots
    'ref_reynolds_number': 5300/2,       # reference Reynolds number
    'ref_reynolds_tau': 180.877575141, # reference Friction Reynolds number
    'reynolds_number': 2650,  # Reynolds number
    'fig_width': 9,           # Figure width
    'fig_height': 6,          # Figure height
    'dpi': 500                # Figure DPI
}

#==============================================================================
# PARAMETER CONFIGURATIONS
#==============================================================================

# Parameter configuration dictionary defining how each flow parameter is processed and plotted
PARAMS = {
    'ux': {
        'ref_file': 'PIPE_Re180_MEAN.dat',
        'ref_cols': ['r', '1-r', 'yplus', 'Ur', 'Ut', 'Uz', 'dUz/dy', 'P'],
        'ref_skiprows': 8,
        'ylabel': r'$u_x^+$',
        'ref_key': 'Uz',
        'scaling': 'mean',
        'description': 'Streamwise velocity component'
    },
    'uy': {
        'ref_file': 'PIPE_Re180_MEAN.dat',
        'ref_cols': ['r', '1-r', 'yplus', 'Ur', 'Ut', 'Uz', 'dUz/dy', 'P'],
        'ref_skiprows': 8,
        'ylabel': r'$u_y^+$',
        'ref_key': 'Ur',  # Not used since reference is zero
        'scaling': 'mean',
        'description': 'Wall-normal velocity component'
    },
    'uz': {
        'ref_file': 'PIPE_Re180_MEAN.dat',
        'ref_cols': ['r', '1-r', 'yplus', 'Ur', 'Ut', 'Uz', 'dUz/dy', 'P'],
        'ref_skiprows': 8,
        'ylabel': r'$u_z^+$',
        'ref_key': 'Uz',  # Not used since reference is zero
        'scaling': 'mean',
        'description': 'Spanwise velocity component'
    },
    'pr': {
        'ref_file': 'PIPE_Re180_MEAN.dat',
        'ref_cols': ['r', '1-r', 'yplus', 'Ur', 'Ut', 'Uz', 'dUz/dy', 'P'],
        'ref_skiprows': 8,
        'ylabel': r'$p^+$',
        'ref_key': 'P',
        'scaling': 'mean',
        'description': 'Pressure'
    },
    'uu': {
        'ref_file': 'PIPE_Re180_RMS.dat',
        'ref_cols': ['r', '1-r', 'yplus', 'urur', 'utut', 'uzuz', 'uruz', 'utuz'],
        'ref_skiprows': 8,
        'ylabel': r'$u_{rms}^+$',
        'ref_key': 'uzuz',
        'scaling': 'rms',
        'description': 'Streamwise velocity fluctuation'
    },
    'vv': {
        'ref_file': 'PIPE_Re180_RMS.dat',
        'ref_cols': ['r', '1-r', 'yplus', 'urur', 'utut', 'uzuz', 'uruz', 'utuz'],
        'ref_skiprows': 8,
        'ylabel': r'$v_{rms}^+$',
        'ref_key': 'urur',
        'scaling': 'rms',
        'description': 'Wall-normal velocity fluctuation'
    },
    'ww': {
        'ref_file': 'PIPE_Re180_RMS.dat',
        'ref_cols': ['r', '1-r', 'yplus', 'urur', 'utut', 'uzuz', 'uruz', 'utuz'],
        'ref_skiprows': 8,
        'ylabel': r'$w_{rms}^+$',
        'ref_key': 'utut',
        'scaling': 'rms',
        'description': 'Spanwise velocity fluctuation'
    },
    'uv': {
        'ref_file': 'PIPE_Re180_RMS.dat',
        'ref_cols': ['r', '1-r', 'yplus', 'urur', 'utut', 'uzuz', 'uruz', 'utuz'],
        'ref_skiprows': 8,
        'ylabel': r'$\overline{u^\prime v^\prime}^+$',
        'ref_key': 'uruz',
        'scaling': 'reynolds',
        'description': 'Reynolds stress component'
    }
}

#==============================================================================
# PLOTTING UTILITIES
#==============================================================================

def setup_matplotlib_style():
    """Configure matplotlib for consistent and professional plotting style"""
    plt.rc('figure', facecolor="white")
    plt.rc('legend', fontsize=15)
    plt.rc('axes', labelsize=16, titlesize=18)
    plt.rc('xtick', labelsize=14)
    plt.rc('ytick', labelsize=14)
    plt.rcParams['legend.loc'] = 'best'

    # Configure colormaps
    cbrg = cm.get_cmap(name='brg', lut=None)
    cgry = cm.get_cmap(name='gray', lut=None)
    cbow = cm.get_cmap(name='gist_rainbow', lut=None)
    markers = ["o", "<", "*", "v", "^", '>', '1', '2', '3', '4', 'x', 's', '8', '+']
    
    return {
        'cbrg': cbrg,
        'cgry': cgry, 
        'cbow': cbow,
        'markers': markers
    }

#==============================================================================
# PIPE FLOW ANALYZER CLASS
#==============================================================================

class PipeFlowAnalyzer:
    """
    Class for analyzing and visualizing pipe flow data.
    
    This class handles loading, processing, and plotting pipe flow simulation data
    for comparison with reference DNS data.
    """
    
    def __init__(self, param_name, config):
        """
        Initialize the analyzer with a parameter configuration and user settings.
        
        Args:
            param_name (str): Name of the parameter to analyze ('ux', 'uy', etc.)
            config (dict): Configuration settings for the analysis
        """
        if param_name not in PARAMS:
            raise ValueError(f"Unknown parameter: {param_name}")
        
        self.param = PARAMS[param_name]
        self.param_name = param_name
        self.config = config
        self.plot_style = setup_matplotlib_style()
        
        # Create output directory if it doesn't exist
        Path(config['output_dir']).mkdir(parents=True, exist_ok=True)
        
        # Initialize the analysis
        self.setup_constants()
        self.load_reference_data()
        self.load_flow_data()
    
    def setup_constants(self):
        """Calculate wall units and other constants for flow scaling"""
        # Initialize Reynolds numbers
        self.Re = self.config['reynolds_number']
        self.Re0 = self.config['ref_reynolds_number']
        self.Ret0 = self.config['ref_reynolds_tau']
        self.utau0 = self.Ret0 / self.Re0
        
        # Calculate wall quantities from the streamwise velocity profile
        ux_file = os.path.join(self.config['data_dir'], 
                            f"domain1_time_space_averaged_ux_{self.config['dns_time']}.dat")
        ux_data = np.genfromtxt(ux_file, names=['j', 'r', 'ux'])
        
        # Calculate dudr at the wall using the last row (near-wall point)
        dudy = ux_data['ux'][-1] / (1.0 - ux_data['r'][-1])
        self.tauw = dudy / self.Re
        self.utau = math.sqrt(abs(self.tauw))
        self.Ret = self.Re * self.utau * 2
        
        self.print_comparison()
        
    def print_comparison(self):
        print(f"\n=== Parameter: {self.param_name} ({self.param['description']}) ===")
        
        # Define format string for better alignment
        format_str = "  {:<10} | {:<12} | {:<12}"
        
        # Print header
        print(format_str.format("Parameter", "Reference", "CHAPSim2"))
        print(format_str.format("---------", "----------", "----------"))
        
        # Print comparison data
        print(format_str.format("Re", f"{self.Re0:.1f}", f"{self.Re:.1f}"))
        print(format_str.format("Ret", f"{self.Ret0:.1f}", f"{self.Ret:.1f}"))
        print(format_str.format("u_tau", f"{self.utau0:.6f}", f"{self.utau:.6f}"))
        print(format_str.format("tau_w", f"{self.utau0 * self.utau0:.6f}", f"{self.tauw:.6f}"))     
    
    def load_reference_data(self):
        """Load reference DNS data from file"""
        ref_path = self.config['ref_dir']
        ref_file = os.path.join(ref_path, self.param['ref_file'])
        
        try:
            self.ref_data = np.genfromtxt(
                ref_file, 
                names=self.param['ref_cols'],
                skip_header=self.param['ref_skiprows']
            )
            print(f"  Reference data loaded from: {ref_file}")
        except FileNotFoundError:
            print(f"  WARNING: Reference data file not found: {ref_file}")
            print(f"  Creating empty reference data structure")
            # Create empty reference data structure to avoid errors
            self.ref_data = np.zeros(1, dtype=[(name, float) for name in self.param['ref_cols']])
    
    def load_flow_data(self):
        """Load and perform initial processing of flow data"""
        dns_path = self.config['data_dir']
        dns_file = os.path.join(dns_path, 
                            f"domain1_time_space_averaged_{self.param_name}_{self.config['dns_time']}.dat")
        
        try:
            data = np.genfromtxt(dns_file, names=['j', 'r', self.param_name])
            print(f"  CHAPSim2 data loaded from: {dns_file}")
            
            # Convert structured array to dictionary for easier manipulation
            self.dns_data = {name: data[name] for name in data.dtype.names}
            
            # Transform y coordinates to match reference data convention
            r = self.dns_data['r']
            self.dns_data['r'] = 1 - r
            
            # Process the data based on parameter type
            self.process_data()
            
        except FileNotFoundError:
            print(f"  ERROR: CHAPSim2 data file not found: {dns_file}")
            raise
    
    def process_data(self):
        """Process the data based on parameter type and scaling"""
        # Calculate yplus (non-dimensional wall distance)
        self.dns_data['yplus'] = self.Ret * self.dns_data['r']
        
        # Apply appropriate scaling based on parameter type
        if self.param_name == 'pr':
            # For pressure, scale by wall shear stress and shift to make wall value zero
            pr_data = self.dns_data[self.param_name] / self.tauw
            pr_data = pr_data - pr_data[0]  # Shift data to make wall value zero
            self.dns_data['scaled'] = pr_data
            
        elif self.param['scaling'] == 'mean':
            # Process mean velocity by scaling with friction velocity
            self.dns_data['scaled'] = self.dns_data[self.param_name] / self.utau
            
        elif self.param['scaling'] == 'rms':
            # Process RMS velocity fluctuations
            self._process_rms_data()
                
        elif self.param['scaling'] == 'reynolds':
            # Process Reynolds stress components
            self._process_reynolds_stress()
    
    def _process_rms_data(self):
        """Process RMS velocity fluctuation data"""
        # Load mean velocities for fluctuation calculations
        ux_file = os.path.join(self.config['data_dir'], 
                            f"domain1_time_space_averaged_ux_{self.config['dns_time']}.dat")
        uz_file = os.path.join(self.config['data_dir'], 
                            f"domain1_time_space_averaged_uz_{self.config['dns_time']}.dat")
        
        # uy is zero in fully developed pipe flow
        uy = np.zeros_like(self.dns_data['r'])
        
        # Load ux data
        ux_data = np.genfromtxt(ux_file, names=['j', 'r', 'ux'])
        ux = ux_data['ux']
        
        # Load uz data
        uz_data = np.genfromtxt(uz_file, names=['j', 'r', 'uz'])
        uz = uz_data['uz']
        
        # Calculate RMS values based on parameter
        if self.param_name == 'uu':
            # u_rms = sqrt(<u'u'>) = sqrt(<uu> - <u><u>)
            self.dns_data['scaled'] = np.sqrt(self.dns_data[self.param_name] - ux * ux) / self.utau
        elif self.param_name == 'vv':
            # v_rms = sqrt(<v'v'>) = sqrt(<vv> - <v><v>)
            self.dns_data['scaled'] = np.sqrt(self.dns_data[self.param_name] - uy * uy) / self.utau
        elif self.param_name == 'ww':
            # w_rms = sqrt(<w'w'>) = sqrt(<ww> - <w><w>)
            self.dns_data['scaled'] = np.sqrt(self.dns_data[self.param_name] - uz * uz) / self.utau
    
    def _process_reynolds_stress(self):
        """Process Reynolds stress component data"""
        # Load mean streamwise velocity
        ux_file = os.path.join(self.config['data_dir'], 
                            f"domain1_time_space_averaged_ux_{self.config['dns_time']}.dat")
        ux_data = np.genfromtxt(ux_file, names=['j', 'r', 'ux'])
        ux = ux_data['ux']
        
        # uy is zero in fully developed pipe flow
        uy = np.zeros_like(self.dns_data['r'])
        
        # Calculate Reynolds stress: <u'v'> = <uv> - <u><v>
        self.dns_data['scaled'] = (self.dns_data[self.param_name] - ux * uy) / (self.utau * self.utau)
    
    def plot_profile(self):
        """Plot the flow profile with reference data comparison"""
        # Create figure
        fig, ax = plt.subplots(figsize=(self.config['fig_width'], self.config['fig_height']), 
                              dpi=self.config['dpi'])
        
        # Set labels
        ax.set_xlabel(r'$y^+$', fontsize=20)
        ax.set_ylabel(self.param['ylabel'], fontsize=20)
        ax.set_title(f"{self.param['description']} Profile", fontsize=22)
        
        # Special handling for uy (reference is zero)
        # if self.param_name == 'uy' or self.param_name == 'uz':
        #     self._plot_zero_reference(ax)
        # else:
        self._plot_reference_data(ax)
        
        # Plot DNS data
        self._plot_dns_data(ax)
        
        # Set x-axis to log scale and range
        ax.set_xscale('log')
        ax.set_xlim(0.1, 500)
        
        # Customize plot
        ax.legend(loc='upper left', ncol=1, labelspacing=0.1, frameon=False, 
                 handlelength=3.2, numpoints=1)
        ax.grid(True, which="both", ls="-", alpha=0.2)
        
        # Save plot
        output_file = os.path.join(self.config['output_dir'], f'pipe_{self.param_name}.png')
        fig.savefig(output_file)
        print(f"  Plot saved to: {output_file}")
        plt.close('all')
    
    def _plot_zero_reference(self, ax):
        """Plot zero reference line for uy and uz profiles"""
        # Generate zero reference line
        yplus_ref = np.logspace(-1, 3, 100)  # Create log-spaced points
        zeros_ref = np.zeros_like(yplus_ref)
        
        # Plot reference data
        ax.plot(
            yplus_ref,
            zeros_ref,
            marker=self.plot_style['markers'][1],
            mfc='none',
            ms=4,
            mec=self.plot_style['cbrg'](0.00),
            color=self.plot_style['cbrg'](0.00),
            linestyle='-',
            label='Reference DNS'
        )
    
    def _plot_reference_data(self, ax):
        """Plot reference data for comparison"""
        # Prepare reference data
        ref_data_plot = self.ref_data[self.param['ref_key']]
        
        # Apply appropriate processing based on parameter type
        if self.param['scaling'] == 'rms':
            # For RMS components, take the square root
            ref_data_plot = np.sqrt(ref_data_plot)
            ref_yplus = self.ref_data['yplus']
        elif self.param['scaling'] == 'reynolds':
            # For Reynolds stress, take absolute value and filter positive yplus
            ref_data_plot = np.abs(ref_data_plot)
            mask = self.ref_data['yplus'] > 0
            ref_data_plot = ref_data_plot[mask]
            ref_yplus = self.ref_data['yplus'][mask]
        else:
            # For mean profiles
            ref_yplus = self.ref_data['yplus']
        
        # Plot reference data
        ax.plot(
            ref_yplus,
            ref_data_plot,
            marker=self.plot_style['markers'][1],
            mfc='none',
            ms=4,
            mec=self.plot_style['cbrg'](0.00),
            color=self.plot_style['cbrg'](0.00),
            linestyle='None',
            label='Reference DNS'
        )
    
    def _plot_dns_data(self, ax):
        """Plot simulation data with appropriate filtering"""
        # Apply special handling for Reynolds stress
        if self.param['scaling'] == 'reynolds':
            # Take absolute value and filter positive yplus
            self.dns_data['scaled'] = np.abs(self.dns_data['scaled'])
            mask = self.dns_data['yplus'] > 0
            plot_yplus = self.dns_data['yplus'][mask]
            plot_scaled = self.dns_data['scaled'][mask]
        else:
            # For other parameters
            plot_yplus = self.dns_data['yplus']
            plot_scaled = self.dns_data['scaled']
        
        # Plot DNS data
        ax.plot(
            plot_yplus,
            plot_scaled,
            marker='none',
            color=self.plot_style['cbrg'](0.50),
            linestyle='-.',
            linewidth=2,
            label='CHAPsim2'
        )
    
    def plot_all_rms(self):
        """Plot all RMS velocity components in one figure"""
        # Create figure
        fig, ax = plt.subplots(figsize=(self.config['fig_width'], self.config['fig_height']), 
                              dpi=self.config['dpi'])
        
        # Set labels
        ax.set_xlabel(r'$y^+$', fontsize=20)
        ax.set_ylabel(r'$u_{i,rms}^+$', fontsize=20)
        ax.set_title("RMS Velocity Fluctuations", fontsize=22)
        
        # Colors and markers for different components
        colors = {'uu': self.plot_style['cbrg'](0.0), 
                 'vv': self.plot_style['cbrg'](0.3), 
                 'ww': self.plot_style['cbrg'](0.6)}
        markers = {'uu': 'o', 'vv': 's', 'ww': '^'}
        labels = {'uu': r'$u_{rms}^+$', 'vv': r'$v_{rms}^+$', 'ww': r'$w_{rms}^+$'}
        
        try:
            # Load reference data for all components
            ref_file = os.path.join(self.config['ref_dir'], self.param['ref_file'])
            ref_data = np.genfromtxt(
                ref_file,
                skip_header=self.param['ref_skiprows'],
                names=self.param['ref_cols']
            )
            
            # Plot reference data for each component
            for comp, ref_key in [('uu', 'uzuz'), ('vv', 'urur'), ('ww', 'utut')]:
                # Calculate RMS from reference data
                ref_rms = np.sqrt(ref_data[ref_key])
                
                ax.plot(
                    ref_data['yplus'],
                    ref_rms,
                    marker=markers[comp],
                    mfc='none',
                    ms=6,
                    mec=colors[comp],
                    color=colors[comp],
                    linestyle='None',
                    markevery=1,
                    label=f'Reference {labels[comp]}'
                )
        except FileNotFoundError:
            print(f"  WARNING: Reference data file not found")
        
        # Load and process DNS data for each component
        for comp in ['uu', 'vv', 'ww']:
            try:
                # Load the data
                dns_file = os.path.join(self.config['data_dir'], 
                                      f"domain1_time_space_averaged_{comp}_{self.config['dns_time']}.dat")
                data = np.genfromtxt(dns_file, names=['j', 'r', comp])
                
                # Transform y coordinates and calculate yplus
                y_transformed = 1 - data['r']
                yplus = self.Re * self.utau * y_transformed
                
                # Load mean velocities for fluctuation calculations
                if comp == 'uu':
                    ux_file = os.path.join(self.config['data_dir'], 
                                        f"domain1_time_space_averaged_ux_{self.config['dns_time']}.dat")
                    ux_data = np.genfromtxt(ux_file, names=['j', 'r', 'ux'])
                    mean_vel = ux_data['ux']
                elif comp == 'vv':
                    mean_vel = np.zeros_like(data['r'])  # uy is zero in pipe flow
                else:  # ww
                    uz_file = os.path.join(self.config['data_dir'], 
                                        f"domain1_time_space_averaged_uz_{self.config['dns_time']}.dat")
                    uz_data = np.genfromtxt(uz_file, names=['j', 'r', 'uz'])
                    mean_vel = uz_data['uz']
                
                # Calculate RMS value
                rms = np.sqrt(data[comp] - mean_vel * mean_vel) / self.utau
                
                # Plot DNS data
                ax.plot(
                    yplus,
                    rms,
                    color=colors[comp],
                    linestyle='-.',
                    linewidth=2,
                    label=f'CHAPsim2 {labels[comp]}'
                )
            except FileNotFoundError:
                print(f"  WARNING: Data file for {comp} not found")
        
        # Set x-axis to log scale and range
        ax.set_xscale('log')
        ax.set_xlim(0.1, 500)
        
        # Customize plot
        ax.legend(loc='upper left', ncol=1, labelspacing=0.1, frameon=False, 
                 handlelength=3.2, numpoints=1)
        ax.grid(True, which="both", ls="-", alpha=0.2)
        
        # Save plot
        output_file = os.path.join(self.config['output_dir'], 'pipe_all_rms.png')
        fig.savefig(output_file)
        print(f"  Combined RMS plot saved to: {output_file}")
        plt.close('all')

#==============================================================================
# MAIN EXECUTION
#==============================================================================

def parse_arguments():
    """Parse command line arguments"""
    parser = argparse.ArgumentParser(
        description='Post-process and visualize pipe flow simulation data',
        formatter_class=argparse.ArgumentDefaultsHelpFormatter
    )
    
    parser.add_argument('--dns-time', type=str, default=DEFAULT_CONFIG['dns_time'],
                       help='Time value for DNS data analysis')
    parser.add_argument('--data-dir', type=str, default=DEFAULT_CONFIG['data_dir'],
                       help='Directory containing CHAPSim2 simulation results')
    parser.add_argument('--ref-dir', type=str, default=DEFAULT_CONFIG['ref_dir'],
                       help='Directory containing reference data files')
    parser.add_argument('--output-dir', type=str, default=DEFAULT_CONFIG['output_dir'],
                       help='Directory for output plots')
    parser.add_argument('--reynolds', type=float, default=DEFAULT_CONFIG['ref_reynolds_number'],
                       help='reference Reynolds number')
    parser.add_argument('--reynolds-tau', type=float, default=DEFAULT_CONFIG['ref_reynolds_tau'],
                       help='reference Friction Reynolds number')
    parser.add_argument('--plot-size', type=str, default=f"{DEFAULT_CONFIG['fig_width']}x{DEFAULT_CONFIG['fig_height']}",
                       help='Figure size in format WIDTHxHEIGHT')
    parser.add_argument('--dpi', type=int, default=DEFAULT_CONFIG['dpi'],
                       help='Figure DPI')
    parser.add_argument('--parameters', type=str, default='all',
                       help='Comma-separated list of parameters to plot (or "all")')
    
    args = parser.parse_args()
    
    # Parse figure size
    width, height = map(int, args.plot_size.split('x'))
    
    # Build configuration dictionary
    config = {
        'dns_time': args.dns_time,
        'data_dir': args.data_dir,
        'ref_dir': args.ref_dir,
        'output_dir': args.output_dir,
        'reynolds_number': args.reynolds,
        'reynolds_tau': args.reynolds_tau,
        'fig_width': width,
        'fig_height': height,
        'dpi': args.dpi
    }
    
    # Parse parameters to plot
    if args.parameters.lower() == 'all':
        parameters = list(PARAMS.keys())
    else:
        parameters = [p.strip() for p in args.parameters.split(',')]
        # Validate parameters
        for p in parameters:
            if p not in PARAMS:
                print(f"WARNING: Unknown parameter '{p}'. Skipping.")
                parameters.remove(p)
    
    return config, parameters

def main():
    """Main function to execute the post-processing and visualization"""
    # Parse command line arguments
    config, parameters = parse_arguments()
    
    print("\n===== PIPE FLOW POST-PROCESSING AND VISUALIZATION =====")
    print(f"Configuration:")
    print(f"- DNS iteration: {config['dns_time']}")
    print(f"- Data directory: {config['data_dir']}")
    print(f"- Reference directory: {config['ref_dir']}")
    
    # Plot individual components
    for param_name in ['ux', 'uy', 'uz', 'pr', 'uu', 'vv', 'ww', 'uv']:
        analyzer = PipeFlowAnalyzer(param_name, DEFAULT_CONFIG)
        analyzer.plot_profile()
    
    # Plot all RMS components together
    analyzer = PipeFlowAnalyzer('ux', DEFAULT_CONFIG)  # Just need one instance for common values
    analyzer.plot_all_rms()

if __name__ == "__main__":
    main()
