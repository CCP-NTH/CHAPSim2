import numpy as np
import matplotlib.pyplot as plt


def get_int_input(prompt, default):
    user_input = input(f"{prompt} (default = {default}): ").strip()
    return int(user_input) if user_input else default

def get_yes_no_input(prompt, default='n'):
    user_input = input(f"{prompt} [y/N]: ").strip().lower()
    if user_input == '':
        user_input = default.lower()
    return user_input == 'y'

# Ask user for input
skip_rows = get_int_input("Enter number of lines to skip at the top of the files: ", 10)
stride = get_int_input("Enter the sampling stride (plot every Nth point): ", 5)
include_thermo = get_yes_no_input("Include thermo information (gx_bulk, T_bulk, H_bulk)?")

# Set white background and black edges
plt.rcParams['figure.facecolor'] = 'white'
plt.rcParams['axes.facecolor'] = 'white'
plt.rcParams['savefig.facecolor'] = 'white'
plt.rcParams['axes.edgecolor'] = 'black'
plt.rcParams['axes.labelcolor'] = 'black'
plt.rcParams['xtick.color'] = 'black'
plt.rcParams['ytick.color'] = 'black'

# Load data
bulk_data = np.loadtxt('domain1_monitor_bulk_history.log', skiprows=skip_rows)
mass_data = np.loadtxt('domain1_monitor_change_history.log', skiprows=skip_rows)

# Subsample bulk_data
time_bulk = bulk_data[::stride, 0]
MKE = bulk_data[::stride, 1]
ux_bulk = bulk_data[::stride, 2]

# Optional thermo data
if include_thermo:
    gx_bulk = bulk_data[::stride, 3]
    T_bulk = bulk_data[::stride, 4]
    H_bulk = bulk_data[::stride, 5]

# Subsample mass_data
time_mass = mass_data[::stride, 0]
mass_cons_1 = mass_data[::stride, 1]
mass_cons_2 = mass_data[::stride, 2]
mass_cons_3 = mass_data[::stride, 3]
dmdt = mass_data[::stride, 4]
dtkedt = mass_data[::stride, 5]

# Define number of subplots
n_rows = 3 if include_thermo else 2
fig, axes = plt.subplots(n_rows, 3, figsize=(20, 4.5 * n_rows))
axes = axes.flatten()

plt.style.use('bmh')

# Plot 1: Mass Conservation
axes[0].plot(time_mass, mass_cons_1, 'b-', label='mass_cons[1]')
axes[0].plot(time_mass, mass_cons_2, 'r-', label='mass_cons[2]')
axes[0].plot(time_mass, mass_cons_3, 'g-', label='mass_cons[3]')
axes[0].set_yscale('log')
axes[0].set_xlabel('Time')
axes[0].set_ylabel('Mass Conservation')
axes[0].grid(True, color='gray', alpha=0.3)
axes[0].legend()

# Plot 2: MKE
axes[1].plot(time_bulk, MKE, 'b-', label='MKE')
axes[1].set_xlabel('Time')
axes[1].set_ylabel('MKE')
axes[1].grid(True, color='gray', alpha=0.3)
axes[1].legend()

# Plot 3: dtke/dt
axes[2].plot(time_mass, dtkedt, 'orange', label='dtke/dt')
axes[2].set_xlabel('Time')
axes[2].set_ylabel('dtke/dt')
axes[2].grid(True, color='gray', alpha=0.3)
axes[2].legend()

# Plot 4: dm/dt
axes[3].plot(time_mass, dmdt, 'purple', label='dm/dt')
axes[3].set_xlabel('Time')
axes[3].set_ylabel('dm/dt')
axes[3].grid(True, color='gray', alpha=0.3)
axes[3].legend()

# Plot 5: ux_bulk
axes[4].plot(time_bulk, ux_bulk, 'r-', label='ux_bulk')
axes[4].set_xlabel('Time')
axes[4].set_ylabel('ux_bulk')
axes[4].grid(True, color='gray', alpha=0.3)
axes[4].legend()

if include_thermo:
    # Plot 6: gx_bulk
    axes[5].plot(time_bulk, gx_bulk, 'c-', label='gx_bulk')
    axes[5].set_xlabel('Time')
    axes[5].set_ylabel('gx_bulk')
    axes[5].grid(True, color='gray', alpha=0.3)
    axes[5].legend()

    # Plot 7: T_bulk
    axes[6].plot(time_bulk, T_bulk, 'brown', label='T_bulk')
    axes[6].set_xlabel('Time')
    axes[6].set_ylabel('T_bulk')
    axes[6].grid(True, color='gray', alpha=0.3)
    axes[6].legend()

    # Plot 8: H_bulk
    axes[7].plot(time_bulk, H_bulk, 'olive', label='H_bulk')
    axes[7].set_xlabel('Time')
    axes[7].set_ylabel('H_bulk')
    axes[7].grid(True, color='gray', alpha=0.3)
    axes[7].legend()

plt.tight_layout()
fig.suptitle('Monitor Variables History', fontsize=16, y=1.02)
plt.savefig('monitor_history.png', bbox_inches='tight', dpi=300)
# plt.show()
