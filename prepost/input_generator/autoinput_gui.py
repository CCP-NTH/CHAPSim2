#!/usr/bin/env python3
"""CHAPSim2 GUI Auto Input Generator - User-friendly configuration interface."""

try:
    import tkinter as tk
    from tkinter import ttk, messagebox
except ImportError as exc:
    tk = None
    ttk = None
    messagebox = None
    TKINTER_IMPORT_ERROR = exc
else:
    TKINTER_IMPORT_ERROR = None
import configparser
import math
from enum import Enum
from pathlib import Path

# Constants
PI = round(math.pi, 6)
TWO_PI = 2.0 * PI
DEFAULT_FILENAME = "input_chapsim_gui.ini"
SCRIPT_DIR = Path(__file__).resolve().parent
TEMPLATE_PATH = SCRIPT_DIR / "input_chapsim_complete.ini"
DEFAULT_VISU_SKIP = "1,1,1"
DEFAULT_STAT_SKIP = "1,1,1"
WALL_BC_CASES = {1, 3, 5}
LOGO_PATH = Path(__file__).resolve().parents[1] / "chapsim_logo.png"


class Case(Enum):
    CHANNEL = 1
    PIPE = 2
    ANNULAR = 3
    TGV3D = 4
    DUCT = 5


class Init(Enum):
    RESTART = 0
    RANDOM = 2
    INLET = 3
    GIVEN = 4
    POISEUILLE = 5
    FUNCTION = 6
    GVBCLN = 7
    GVBCSMOOTH = 8


class Stretching(Enum):
    NONE = 0
    CENTRE = 1
    SIDE2 = 2
    BOTTOM = 3
    TOP = 4


class BC(Enum):
    INTERIOR = 0
    PERIODIC = 1
    SYMM = 2
    ASYMM = 3
    DIRICHLET = 4
    NEUMANN = 5
    INTRPL = 6
    CONVOL = 7
    TURGEN = 8
    PROFL = 9
    DATABS = 10
    PARABOLIC = 11
    OTHERS = 12


class Drvfc(Enum):
    NONE = 0
    XMFLUX = 1
    XTAUW = 2
    XDPDX = 3
    ZMFLUX = 4
    ZTAUW = 5
    ZDPDZ = 6


def ensure_template_available():
    """Return the moved complete input template path, failing clearly if absent."""
    if not TEMPLATE_PATH.exists():
        raise FileNotFoundError(f"Template input file not found: {TEMPLATE_PATH}")
    return TEMPLATE_PATH


def bool_to_string(value):
    """Converts 0/1 to Fortran boolean strings."""
    return ".true." if value else ".false."


class CustomConfigParser(configparser.ConfigParser):
    """Custom ConfigParser that formats output with space after '='."""

    def __init__(self):
        super().__init__(interpolation=None)
        self.optionxform = str

    def write(self, fp):
        for section in self.sections():
            fp.write(f"[{section}]\n")
            for key, value in self.items(section):
                fp.write(f"{key}= {value}\n")
            fp.write("\n")


class CHAPSimGUI:
    def __init__(self, root):
        if TKINTER_IMPORT_ERROR is not None:
            raise RuntimeError(
                "Tkinter is required to run the GUI input generator. "
                f"Original import error: {TKINTER_IMPORT_ERROR}"
            )
        self.root = root
        self.root.title("CHAPSim2 Input Generator")
        self.root.geometry("1100x850")

        self.template_path = ensure_template_available()
        self.create_logo_header()

        # Create notebook for tabs
        self.notebook = ttk.Notebook(root)
        self.notebook.pack(fill=tk.BOTH, expand=True, padx=10, pady=10)

        # State variables
        self.icase = tk.IntVar(value=1)
        self.ithermo = tk.BooleanVar(value=False)
        self.imhd = tk.BooleanVar(value=False)
        self.is_restart = tk.BooleanVar(value=False)
        self.is_write = tk.BooleanVar(value=False)
        self.iinlet = tk.IntVar(value=1)  # 1 = periodic (default)

        # Create tabs (order matters - mesh and bc must exist before domain triggers updates)
        self.create_process_tab()
        self.create_decomp_tab()
        self.create_flow_tab()
        self.create_thermo_tab()
        self.create_mhd_tab()
        self.create_mesh_tab()
        self.create_bc_tab()
        self.create_scheme_tab()
        self.create_simcontrol_tab()
        self.create_io_tab()
        self.create_probe_tab()
        # Create domain tab last so it can trigger updates to other tabs
        self.create_domain_tab()

        # Generate button
        button_frame = ttk.Frame(root)
        button_frame.pack(fill=tk.X, padx=10, pady=10)

        ttk.Button(
            button_frame, text="Generate INI File", command=self.generate_ini
        ).pack(side=tk.LEFT, padx=5)
        ttk.Button(button_frame, text="Exit", command=root.quit).pack(
            side=tk.RIGHT, padx=5
        )

    def create_logo_header(self):
        """Display the CHAPSim logo when the PNG asset is available."""
        self.logo_image = None
        if not LOGO_PATH.exists():
            return

        try:
            self.logo_image = tk.PhotoImage(file=str(LOGO_PATH))
            max_width = 360
            if self.logo_image.width() > max_width:
                factor = math.ceil(self.logo_image.width() / max_width)
                self.logo_image = self.logo_image.subsample(factor, factor)

            header = ttk.Frame(self.root)
            header.pack(fill=tk.X, padx=10, pady=(10, 0))
            ttk.Label(header, image=self.logo_image).pack(side=tk.LEFT)
            ttk.Label(
                header,
                text="CHAPSim2 Input Generator",
                font=("Arial", 16, "bold"),
            ).pack(side=tk.LEFT, padx=12)
        except tk.TclError:
            self.logo_image = None

    def create_labeled_input(
        self, parent, label_text, default_value, row, col=0, input_type="str", width=20
    ):
        """Helper to create labeled input fields."""
        if label_text:  # Only create label if text provided
            ttk.Label(parent, text=label_text).grid(
                row=row, column=col, sticky=tk.W, padx=5, pady=5
            )

        if input_type == "bool":
            var = tk.BooleanVar(value=int(default_value))
            widget = ttk.Checkbutton(parent, variable=var)
            widget.grid(row=row, column=col + 1, sticky=tk.W, padx=5, pady=5)
            return (var, widget)
        elif input_type == "choice":
            var = tk.StringVar(value=str(default_value))
            widget = ttk.Combobox(parent, textvariable=var, state="readonly", width=width)
            widget.grid(row=row, column=col + 1, sticky=tk.W, padx=5, pady=5)
            return (var, widget)
        else:
            var = tk.StringVar(value=str(default_value))
            widget = ttk.Entry(parent, textvariable=var, width=width)
            widget.grid(row=row, column=col + 1, sticky=tk.W, padx=5, pady=5)
            return (var, widget)

    def set_enabled(self, widget, enabled):
        """Enable or disable a widget."""
        if widget:
            state = "normal" if enabled else "disabled"
            if isinstance(widget, ttk.Entry):
                widget.config(state=state)
            elif isinstance(widget, ttk.Combobox):
                widget.config(state="readonly" if enabled else "disabled")
            elif isinstance(widget, ttk.Checkbutton):
                widget.config(state=state)

    def create_process_tab(self):
        """Process settings tab."""
        tab = ttk.Frame(self.notebook)
        self.notebook.add(tab, text="Process")

        self.is_prerun, _ = self.create_labeled_input(
            tab, "Enable prerun only?", 0, 0, 0, "bool"
        )
        self.is_postprocess, _ = self.create_labeled_input(
            tab, "Enable postprocess?", 0, 1, 0, "bool"
        )

    def create_decomp_tab(self):
        """Decomposition settings tab."""
        tab = ttk.Frame(self.notebook)
        self.notebook.add(tab, text="Decomposition")

        self.is_decomp = tk.BooleanVar(value=True)
        ttk.Label(tab, text="Auto domain decomposition?").grid(
            row=0, column=0, sticky=tk.W, padx=5, pady=5
        )
        self.is_decomp_check = ttk.Checkbutton(
            tab, variable=self.is_decomp, command=self.on_decomp_changed
        )
        self.is_decomp_check.grid(row=0, column=1, sticky=tk.W, padx=5, pady=5)

        self.p_row, self.p_row_w = self.create_labeled_input(
            tab, "Subdomain division Y", 0, 1
        )
        self.p_col, self.p_col_w = self.create_labeled_input(
            tab, "Subdomain division Z", 0, 2
        )

        self.on_decomp_changed()

    def on_decomp_changed(self, *args):
        """Handle decomposition checkbox changes."""
        enabled = not self.is_decomp.get()
        self.set_enabled(self.p_row_w, enabled)
        self.set_enabled(self.p_col_w, enabled)

    def create_domain_tab(self):
        """Domain settings tab."""
        tab = ttk.Frame(self.notebook)
        self.notebook.add(tab, text="Domain")

        cases = ["1:Channel", "2:Pipe", "3:Annular", "4:TGV3D", "5:DUCT"]
        self.icase_var, self.icase_combo = self.create_labeled_input(
            tab, "Simulation case", "1:Channel", 0, 0, "choice"
        )
        self.icase_combo["values"] = cases
        self.icase_combo.bind("<<ComboboxSelected>>", lambda e: self.on_case_changed())

        self.lxx, self.lxx_w = self.create_labeled_input(
            tab, "Streamwise length (Lx/h)", TWO_PI, 1
        )
        self.lzz, self.lzz_w = self.create_labeled_input(
            tab, "Spanwise length (Lz/h)", PI, 2
        )
        self.lyb, self.lyb_w = self.create_labeled_input(
            tab, "Bottom boundary", -1.0, 3
        )
        self.lyt, self.lyt_w = self.create_labeled_input(tab, "Top boundary", 1.0, 4)

        # Trigger initial update after all widgets are created
        self.root.after(100, self.on_case_changed)

    def on_case_changed(self):
        """Handle case selection changes - updates all dependent defaults."""
        case_str = self.icase_var.get()
        case_num = int(case_str.split(":")[0])
        self.icase.set(case_num)

        # Enable all first
        self.set_enabled(self.lxx_w, True)
        self.set_enabled(self.lzz_w, True)
        self.set_enabled(self.lyb_w, True)
        self.set_enabled(self.lyt_w, True)

        # Set defaults based on case
        if case_num == Case.CHANNEL.value:
            self.lxx.set(str(TWO_PI))
            self.lzz.set(str(PI))
            self.lyt.set("1.0")
            self.lyb.set("-1.0")
            self.set_enabled(self.lyb_w, False)
            self.set_enabled(self.lyt_w, False)
        elif case_num == Case.PIPE.value:
            self.lxx.set(str(TWO_PI))
            self.lzz.set(str(TWO_PI))
            self.lyt.set("1.0")
            self.lyb.set("0.0")
            self.set_enabled(self.lzz_w, False)
            self.set_enabled(self.lyb_w, False)
            self.set_enabled(self.lyt_w, False)
        elif case_num == Case.ANNULAR.value:
            self.lxx.set(str(TWO_PI))
            self.lzz.set(str(TWO_PI))
            self.lyt.set("1.0")
            self.set_enabled(self.lzz_w, False)
            self.set_enabled(self.lyt_w, False)
        elif case_num == Case.TGV3D.value:
            self.lxx.set(str(TWO_PI))
            self.lzz.set(str(TWO_PI))
            self.lyb.set(str(-PI))
            self.lyt.set(str(PI))
            self.set_enabled(self.lxx_w, False)
            self.set_enabled(self.lzz_w, False)
            self.set_enabled(self.lyb_w, False)
            self.set_enabled(self.lyt_w, False)
        elif case_num == Case.DUCT.value:
            self.lxx.set("2.0")
            self.lzz.set("12.0")
            self.lyb.set("-1.0")
            self.lyt.set("1.0")
            self.set_enabled(self.lyb_w, False)
            self.set_enabled(self.lyt_w, False)

        # Update mesh stretching defaults
        self.update_mesh_defaults()
        
        # Update BC defaults
        self.update_bc_defaults()
        
        # Update flow defaults
        self.update_flow_defaults()

    def create_flow_tab(self):
        """Flow settings tab."""
        tab = ttk.Frame(self.notebook)
        self.notebook.add(tab, text="Flow")

        ttk.Label(tab, text="Flow restart?").grid(
            row=0, column=0, sticky=tk.W, padx=5, pady=5
        )
        self.is_restart_check = ttk.Checkbutton(
            tab, variable=self.is_restart, command=self.on_restart_changed
        )
        self.is_restart_check.grid(row=0, column=1, sticky=tk.W, padx=5, pady=5)

        self.irestartfrom, self.irestartfrom_w = self.create_labeled_input(
            tab, "Restart from iteration", 2000, 1
        )

        self.velo1, self.velo1_w = self.create_labeled_input(
            tab, "Initial velocity X", 0.0, 3
        )
        self.velo2, self.velo2_w = self.create_labeled_input(
            tab, "Initial velocity Y", 0.0, 4
        )
        self.velo3, self.velo3_w = self.create_labeled_input(
            tab, "Initial velocity Z", 0.0, 5
        )
        self.noiselevel, self.noiselevel_w = self.create_labeled_input(
            tab, "Noise level (0-1)", 0.25, 6
        )
        self.ren, self.ren_w = self.create_labeled_input(
            tab, "Reynolds number", 2800, 7
        )
        self.reni, self.reni_w = self.create_labeled_input(
            tab, "Initial Reynolds number", 20000, 8
        )
        self.nreni, self.nreni_w = self.create_labeled_input(
            tab, "Iterations for initial Re", 10000, 9
        )

        init_options = [
            "2:Random",
            "3:Inlet",
            "4:Const",
            "5:Poiseuille",
            "6:Function",
        ]
        self.initfl, self.initfl_w = self.create_labeled_input(
            tab, "Flow initialization", "5:Poiseuille", 2, 0, "choice"
        )
        self.initfl_w["values"] = init_options

        self.on_restart_changed()

    def update_flow_defaults(self):
        """Update flow defaults based on case."""
        # Safety check - ensure flow widgets exist
        if not hasattr(self, 'noiselevel'):
            return
            
        case_num = self.icase.get()

        if hasattr(self, "initfl"):
            if case_num == Case.TGV3D.value:
                self.initfl.set("6:Function")
                self.set_enabled(self.initfl_w, False)
            else:
                if self.initfl.get().startswith("6"):
                    self.initfl.set("5:Poiseuille")
                self.set_enabled(self.initfl_w, not self.is_restart.get())
        
        # Update noise level visibility
        if case_num == Case.TGV3D.value:
            self.noiselevel.set("0.0")
            self.set_enabled(self.noiselevel_w, False)
        else:
            self.noiselevel.set("0.25")
            self.set_enabled(self.noiselevel_w, not self.is_restart.get())
        
        # Update reni/nreni for TGV3D
        if case_num == Case.TGV3D.value:
            ren_val = self.ren.get()
            self.reni.set(ren_val)
            self.nreni.set("0")
            self.set_enabled(self.reni_w, False)
            self.set_enabled(self.nreni_w, False)
        else:
            if self.reni.get() == "0" or self.reni.get() == self.ren.get():
                self.reni.set("20000")
            if self.nreni.get() == "0":
                self.nreni.set("10000")
            self.set_enabled(self.reni_w, not self.is_restart.get())
            self.set_enabled(self.nreni_w, not self.is_restart.get())

    def on_restart_changed(self):
        """Handle restart checkbox changes."""
        enabled = self.is_restart.get()
        self.set_enabled(self.irestartfrom_w, enabled)
        self.set_enabled(self.velo1_w, not enabled)
        self.set_enabled(self.velo2_w, not enabled)
        self.set_enabled(self.velo3_w, not enabled)
        if hasattr(self, "initfl_w"):
            self.set_enabled(self.initfl_w, not enabled and self.icase.get() != Case.TGV3D.value)
        
        case_num = self.icase.get()
        if case_num != Case.TGV3D.value:
            self.set_enabled(self.noiselevel_w, not enabled)
            self.set_enabled(self.reni_w, not enabled)
            self.set_enabled(self.nreni_w, not enabled)

    def create_thermo_tab(self):
        """Thermal settings tab."""
        tab = ttk.Frame(self.notebook)
        self.notebook.add(tab, text="Thermo")

        ttk.Label(tab, text="Enable thermal field?").grid(
            row=0, column=0, sticky=tk.W, padx=5, pady=5
        )
        self.ithermo_check = ttk.Checkbutton(
            tab, variable=self.ithermo, command=self.on_thermo_changed
        )
        self.ithermo_check.grid(row=0, column=1, sticky=tk.W, padx=5, pady=5)

        self.icht, self.icht_w = self.create_labeled_input(
            tab, "Conjugate heat transfer?", 0, 1, 0, "bool"
        )

        gravity_options = ["0:None", "1:+X", "-1:-X", "2:+Y", "-2:-Y", "3:+Z", "-3:-Z"]
        self.igravity, self.igravity_w = self.create_labeled_input(
            tab, "Gravity direction", "0:None", 2, 0, "choice"
        )
        self.igravity_w["values"] = gravity_options

        fluid_options = [
            "1:scp-H2O",
            "2:scp-CO2",
            "3:sodium",
            "4:lead",
            "5:bismuth",
            "6:LBE",
        ]
        self.ifluid, self.ifluid_w = self.create_labeled_input(
            tab, "Fluid type", "1:scp-H2O", 3, 0, "choice"
        )
        self.ifluid_w["values"] = fluid_options

        self.refl0, self.refl0_w = self.create_labeled_input(
            tab, "Reference length (m)", 0.001, 4
        )
        self.refT0, self.refT0_w = self.create_labeled_input(
            tab, "Reference temperature (K)", 645.15, 5
        )

        inittm_options = [
            "0:Restart",
            "4:Const",
            "6:Function",
            "7:Linear",
            "8:Smooth",
        ]
        self.inittm, self.inittm_w = self.create_labeled_input(
            tab, "Thermal initialization", "4:Const", 6, 0, "choice"
        )
        self.inittm_w["values"] = inittm_options

        self.Tini, self.Tini_w = self.create_labeled_input(
            tab, "Initial temperature (K)", 645.15, 7
        )

        self.buffer_inlet, self.buffer_inlet_w = self.create_labeled_input(
            tab, "Inlet thermal buffer length", 0.0, 8
        )
        self.buffer_outlet, self.buffer_outlet_w = self.create_labeled_input(
            tab, "Outlet thermal buffer length", 0.0, 9
        )
        self.use_qw_ramp, self.use_qw_ramp_w = self.create_labeled_input(
            tab, "Enable wall heat-flux ramp?", 0, 10, 0, "bool"
        )
        self.qw_ramp_start, self.qw_ramp_start_w = self.create_labeled_input(
            tab, "Heat-flux ramp start iteration", 0, 11
        )
        self.qw_ramp_end, self.qw_ramp_end_w = self.create_labeled_input(
            tab, "Heat-flux ramp end iteration", 0, 12
        )

        self.on_thermo_changed()

    def on_thermo_changed(self):
        """Handle thermal checkbox changes."""
        enabled = self.ithermo.get()
        self.set_enabled(self.icht_w, enabled)
        self.set_enabled(self.igravity_w, enabled)
        self.set_enabled(self.ifluid_w, enabled)
        self.set_enabled(self.refl0_w, enabled)
        self.set_enabled(self.refT0_w, enabled)
        self.set_enabled(self.inittm_w, enabled)
        self.set_enabled(self.Tini_w, enabled)
        self.set_enabled(self.buffer_inlet_w, enabled)
        self.set_enabled(self.buffer_outlet_w, enabled)
        self.set_enabled(self.use_qw_ramp_w, enabled)
        self.set_enabled(self.qw_ramp_start_w, enabled)
        self.set_enabled(self.qw_ramp_end_w, enabled)
        self.update_bc_defaults()

    def create_mhd_tab(self):
        """MHD settings tab."""
        tab = ttk.Frame(self.notebook)
        self.notebook.add(tab, text="MHD")

        ttk.Label(tab, text="Enable MHD?").grid(
            row=0, column=0, sticky=tk.W, padx=5, pady=5
        )
        self.imhd_check = ttk.Checkbutton(
            tab, variable=self.imhd, command=self.on_mhd_changed
        )
        self.imhd_check.grid(row=0, column=1, sticky=tk.W, padx=5, pady=5)

        mhd_type = ["1:Stuart", "2:Hartmann"]
        self.mhd_type, self.mhd_type_w = self.create_labeled_input(
            tab, "MHD number type", "2:Hartmann", 1, 0, "choice"
        )
        self.mhd_type_w["values"] = mhd_type

        self.NS, self.NS_w = self.create_labeled_input(tab, "Stuart number", 10.0, 2)
        self.NH, self.NH_w = self.create_labeled_input(tab, "Hartmann number", 10.0, 3)
        self.b1, self.b1_w = self.create_labeled_input(tab, "Magnetic field X", 0.0, 4)
        self.b2, self.b2_w = self.create_labeled_input(tab, "Magnetic field Y", 1.0, 5)
        self.b3, self.b3_w = self.create_labeled_input(tab, "Magnetic field Z", 0.0, 6)

        self.on_mhd_changed()

    def on_mhd_changed(self):
        """Handle MHD checkbox changes."""
        enabled = self.imhd.get()
        self.set_enabled(self.mhd_type_w, enabled)
        self.set_enabled(self.NS_w, enabled)
        self.set_enabled(self.NH_w, enabled)
        self.set_enabled(self.b1_w, enabled)
        self.set_enabled(self.b2_w, enabled)
        self.set_enabled(self.b3_w, enabled)

    def create_mesh_tab(self):
        """Mesh settings tab."""
        tab = ttk.Frame(self.notebook)
        self.notebook.add(tab, text="Mesh")

        self.ncx, self.ncx_w = self.create_labeled_input(tab, "Cells in X", 64, 0)
        self.ncy, self.ncy_w = self.create_labeled_input(tab, "Cells in Y", 64, 1)
        self.ncz, self.ncz_w = self.create_labeled_input(tab, "Cells in Z", 64, 2)

        stretch_options = ["0:None", "1:Centre", "2:2-sides", "3:Bottom", "4:Top"]
        self.istret, self.istret_w = self.create_labeled_input(
            tab, "Grid clustering", "2:2-sides", 3, 0, "choice"
        )
        self.istret_w["values"] = stretch_options
        self.istret_w.bind(
            "<<ComboboxSelected>>", lambda e: self.on_stretching_changed()
        )

        stretch_method_options = ["uniform", "3fmd", "tanh", "powl"]
        self.rstret1, self.rstret1_w = self.create_labeled_input(
            tab, "Stretching method", "3fmd", 4, 0, "choice"
        )
        self.rstret1_w["values"] = stretch_method_options
        self.rstret2, self.rstret2_w = self.create_labeled_input(
            tab, "Stretching factor", 0.12, 5
        )

        self.on_stretching_changed()

    def update_mesh_defaults(self):
        """Update mesh stretching defaults based on case."""
        # Safety check - ensure mesh widgets exist
        if not hasattr(self, 'istret'):
            return
            
        case_num = self.icase.get()
        
        if case_num in [Case.CHANNEL.value, Case.DUCT.value, Case.ANNULAR.value]:
            self.istret.set("2:2-sides")
            self.rstret1.set("3fmd")
            self.rstret2.set("0.12")
        elif case_num == Case.PIPE.value:
            self.istret.set("4:Top")
            self.rstret1.set("tanh")
            self.rstret2.set("0.15")
        elif case_num == Case.TGV3D.value:
            self.istret.set("0:None")
            self.rstret1.set("uniform")
            self.rstret2.set("0.0")
        
        self.on_stretching_changed()

    def on_stretching_changed(self):
        """Handle stretching selection changes."""
        istret_val = int(self.istret.get().split(":")[0])
        enabled = istret_val != 0
        self.set_enabled(self.rstret1_w, enabled)
        self.set_enabled(self.rstret2_w, enabled)
        
        # Update stretching method defaults based on case
        if enabled:
            case_num = self.icase.get()
            if case_num in [Case.CHANNEL.value, Case.DUCT.value]:
                if self.rstret1.get() not in ["3fmd", "tanh", "powl"]:
                    self.rstret1.set("3fmd")
                if float(self.rstret2.get()) < 0.01:
                    self.rstret2.set("0.12")
            elif case_num in [Case.PIPE.value, Case.ANNULAR.value]:
                if self.rstret1.get() not in ["3fmd", "tanh", "powl"]:
                    self.rstret1.set("tanh")
                if float(self.rstret2.get()) < 0.01:
                    self.rstret2.set("0.15")

    def create_bc_tab(self):
        """Boundary condition settings tab."""
        tab = ttk.Frame(self.notebook)
        self.notebook.add(tab, text="BC")

        # Create a scrollable frame
        canvas = tk.Canvas(tab)
        scrollbar = ttk.Scrollbar(tab, orient="vertical", command=canvas.yview)
        scrollable_frame = ttk.Frame(canvas)

        scrollable_frame.bind(
            "<Configure>", lambda e: canvas.configure(scrollregion=canvas.bbox("all"))
        )

        canvas.create_window((0, 0), window=scrollable_frame, anchor="nw")
        canvas.configure(yscrollcommand=scrollbar.set)

        # BC type options
        bc_options = [
            "0:Interior",
            "1:Periodic",
            "2:Symm",
            "3:Asymm",
            "4:Dirichlet",
            "5:Neumann",
            "6:Intrpl",
            "7:ConvOut",
            "8:TurGen",
            "9:Profile",
            "10:Database",
            "11:Parabolic",
            "12:Others"
        ]

        # Store all BC widgets in dictionary
        self.bc_widgets = {}
        row = 0

        # Headers
        ttk.Label(scrollable_frame, text="Boundary", font=("Arial", 10, "bold")).grid(
            row=row, column=0, padx=5, pady=5
        )
        ttk.Label(scrollable_frame, text="BC Type 1", font=("Arial", 10, "bold")).grid(
            row=row, column=1, padx=5, pady=5
        )
        ttk.Label(scrollable_frame, text="BC Type 2", font=("Arial", 10, "bold")).grid(
            row=row, column=2, padx=5, pady=5
        )
        ttk.Label(scrollable_frame, text="Value 1", font=("Arial", 10, "bold")).grid(
            row=row, column=3, padx=5, pady=5
        )
        ttk.Label(scrollable_frame, text="Value 2", font=("Arial", 10, "bold")).grid(
            row=row, column=4, padx=5, pady=5
        )
        row += 1

        # X-direction BC
        ttk.Label(scrollable_frame, text="X-direction (ifbcx_u/v/w)").grid(
            row=row, column=0, sticky=tk.W, padx=5, pady=5
        )
        self.bc_widgets["ifbcx_u_1"], _ = self._create_bc_combo(
            scrollable_frame, bc_options, "1", row, 1
        )
        self.bc_widgets["ifbcx_u_2"], _ = self._create_bc_combo(
            scrollable_frame, bc_options, "1", row, 2
        )
        self.bc_widgets["ifbcx_u_v1"], _ = self.create_labeled_input(
            scrollable_frame, "", "0.0", row, 3, "str", width=10
        )
        self.bc_widgets["ifbcx_u_v2"], _ = self.create_labeled_input(
            scrollable_frame, "", "0.0", row, 4, "str", width=10
        )
        row += 1

        ttk.Label(scrollable_frame, text="X-direction (ifbcx_p)").grid(
            row=row, column=0, sticky=tk.W, padx=5, pady=5
        )
        self.bc_widgets["ifbcx_p_1"], _ = self._create_bc_combo(
            scrollable_frame, bc_options, "1", row, 1
        )
        self.bc_widgets["ifbcx_p_2"], _ = self._create_bc_combo(
            scrollable_frame, bc_options, "1", row, 2
        )
        self.bc_widgets["ifbcx_p_v1"], _ = self.create_labeled_input(
            scrollable_frame, "", "0.0", row, 3, "str", width=10
        )
        self.bc_widgets["ifbcx_p_v2"], _ = self.create_labeled_input(
            scrollable_frame, "", "0.0", row, 4, "str", width=10
        )
        row += 1

        ttk.Label(scrollable_frame, text="X-direction (ifbcx_T)").grid(
            row=row, column=0, sticky=tk.W, padx=5, pady=5
        )
        self.bc_widgets["ifbcx_T_1"], _ = self._create_bc_combo(
            scrollable_frame, bc_options, "1", row, 1
        )
        self.bc_widgets["ifbcx_T_2"], _ = self._create_bc_combo(
            scrollable_frame, bc_options, "1", row, 2
        )
        self.bc_widgets["ifbcx_T_v1"], _ = self.create_labeled_input(
            scrollable_frame, "", "0.0", row, 3, "str", width=10
        )
        self.bc_widgets["ifbcx_T_v2"], _ = self.create_labeled_input(
            scrollable_frame, "", "0.0", row, 4, "str", width=10
        )
        row += 1

        # Y-direction BC
        ttk.Label(scrollable_frame, text="Y-direction (ifbcy_u/v/w)").grid(
            row=row, column=0, sticky=tk.W, padx=5, pady=5
        )
        self.bc_widgets["ifbcy_u_1"], _ = self._create_bc_combo(
            scrollable_frame, bc_options, "1", row, 1
        )
        self.bc_widgets["ifbcy_u_2"], _ = self._create_bc_combo(
            scrollable_frame, bc_options, "1", row, 2
        )
        self.bc_widgets["ifbcy_u_v1"], _ = self.create_labeled_input(
            scrollable_frame, "", "0.0", row, 3, "str", width=10
        )
        self.bc_widgets["ifbcy_u_v2"], _ = self.create_labeled_input(
            scrollable_frame, "", "0.0", row, 4, "str", width=10
        )
        row += 1

        ttk.Label(scrollable_frame, text="Y-direction (ifbcy_p)").grid(
            row=row, column=0, sticky=tk.W, padx=5, pady=5
        )
        self.bc_widgets["ifbcy_p_1"], _ = self._create_bc_combo(
            scrollable_frame, bc_options, "1", row, 1
        )
        self.bc_widgets["ifbcy_p_2"], _ = self._create_bc_combo(
            scrollable_frame, bc_options, "1", row, 2
        )
        self.bc_widgets["ifbcy_p_v1"], _ = self.create_labeled_input(
            scrollable_frame, "", "0.0", row, 3, "str", width=10
        )
        self.bc_widgets["ifbcy_p_v2"], _ = self.create_labeled_input(
            scrollable_frame, "", "0.0", row, 4, "str", width=10
        )
        row += 1

        ttk.Label(scrollable_frame, text="Y-direction (ifbcy_T)").grid(
            row=row, column=0, sticky=tk.W, padx=5, pady=5
        )
        self.bc_widgets["ifbcy_T_1"], _ = self._create_bc_combo(
            scrollable_frame, bc_options, "1", row, 1
        )
        self.bc_widgets["ifbcy_T_2"], _ = self._create_bc_combo(
            scrollable_frame, bc_options, "1", row, 2
        )
        self.bc_widgets["ifbcy_T_v1"], _ = self.create_labeled_input(
            scrollable_frame, "", "0.0", row, 3, "str", width=10
        )
        self.bc_widgets["ifbcy_T_v2"], _ = self.create_labeled_input(
            scrollable_frame, "", "0.0", row, 4, "str", width=10
        )
        row += 1

        # Z-direction BC
        ttk.Label(scrollable_frame, text="Z-direction (ifbcz_u/v/w)").grid(
            row=row, column=0, sticky=tk.W, padx=5, pady=5
        )
        self.bc_widgets["ifbcz_u_1"], _ = self._create_bc_combo(
            scrollable_frame, bc_options, "1", row, 1
        )
        self.bc_widgets["ifbcz_u_2"], _ = self._create_bc_combo(
            scrollable_frame, bc_options, "1", row, 2
        )
        self.bc_widgets["ifbcz_u_v1"], _ = self.create_labeled_input(
            scrollable_frame, "", "0.0", row, 3, "str", width=10
        )
        self.bc_widgets["ifbcz_u_v2"], _ = self.create_labeled_input(
            scrollable_frame, "", "0.0", row, 4, "str", width=10
        )
        row += 1

        ttk.Label(scrollable_frame, text="Z-direction (ifbcz_p)").grid(
            row=row, column=0, sticky=tk.W, padx=5, pady=5
        )
        self.bc_widgets["ifbcz_p_1"], _ = self._create_bc_combo(
            scrollable_frame, bc_options, "1", row, 1
        )
        self.bc_widgets["ifbcz_p_2"], _ = self._create_bc_combo(
            scrollable_frame, bc_options, "1", row, 2
        )
        self.bc_widgets["ifbcz_p_v1"], _ = self.create_labeled_input(
            scrollable_frame, "", "0.0", row, 3, "str", width=10
        )
        self.bc_widgets["ifbcz_p_v2"], _ = self.create_labeled_input(
            scrollable_frame, "", "0.0", row, 4, "str", width=10
        )
        row += 1

        ttk.Label(scrollable_frame, text="Z-direction (ifbcz_T)").grid(
            row=row, column=0, sticky=tk.W, padx=5, pady=5
        )
        self.bc_widgets["ifbcz_T_1"], _ = self._create_bc_combo(
            scrollable_frame, bc_options, "1", row, 1
        )
        self.bc_widgets["ifbcz_T_2"], _ = self._create_bc_combo(
            scrollable_frame, bc_options, "1", row, 2
        )
        self.bc_widgets["ifbcz_T_v1"], _ = self.create_labeled_input(
            scrollable_frame, "", "0.0", row, 3, "str", width=10
        )
        self.bc_widgets["ifbcz_T_v2"], _ = self.create_labeled_input(
            scrollable_frame, "", "0.0", row, 4, "str", width=10
        )
        row += 2

        # Inlet BC type
        ttk.Label(scrollable_frame, text="Streamwise BC type").grid(
            row=row, column=0, sticky=tk.W, padx=5, pady=5
        )
        inlet_options = ["1:Periodic", "4:Dirichlet", "9:Profile", "10:Database"]
        self.iinlet_var, self.iinlet_w = self._create_bc_combo(
            scrollable_frame, inlet_options, "1", row, 1
        )
        self.iinlet_w.bind("<<ComboboxSelected>>", lambda e: self.update_bc_defaults())
        row += 1

        # Flow driving options
        ttk.Label(scrollable_frame, text="Flow driving method").grid(
            row=row, column=0, sticky=tk.W, padx=5, pady=5
        )
        driven_options = [
            "0:NONE",
            "1:XMFLUX",
            "2:XTAUW",
            "3:XDPDX",
            "4:ZMFLUX",
            "5:ZTAUW",
            "6:ZDPDZ"
        ]
        self.idriven, self.idriven_w = self._create_bc_combo(
            scrollable_frame, driven_options, "1:XMFLUX", row, 1
        )
        row += 1

        ttk.Label(scrollable_frame, text="Driving force magnitude").grid(
            row=row, column=0, sticky=tk.W, padx=5, pady=5
        )
        self.drivenCf, self.drivenCf_w = self.create_labeled_input(
            scrollable_frame, "", "0.0", row, 1, "str", width=20
        )

        canvas.pack(side="left", fill="both", expand=True)
        scrollbar.pack(side="right", fill="y")

    def _set_bc_entry(self, base_key, bc1, bc2, value1="0.0", value2="0.0"):
        """Set one GUI BC row."""
        names = {
            BC.INTERIOR.value: "Interior",
            BC.PERIODIC.value: "Periodic",
            BC.SYMM.value: "Symm",
            BC.ASYMM.value: "Asymm",
            BC.DIRICHLET.value: "Dirichlet",
            BC.NEUMANN.value: "Neumann",
            BC.INTRPL.value: "Intrpl",
            BC.CONVOL.value: "ConvOut",
            BC.TURGEN.value: "TurGen",
            BC.PROFL.value: "Profile",
            BC.DATABS.value: "Database",
            BC.PARABOLIC.value: "Parabolic",
            BC.OTHERS.value: "Others",
        }
        self.bc_widgets[f"{base_key}_1"].set(f"{bc1}:{names[bc1]}")
        self.bc_widgets[f"{base_key}_2"].set(f"{bc2}:{names[bc2]}")
        self.bc_widgets[f"{base_key}_v1"].set(str(value1))
        self.bc_widgets[f"{base_key}_v2"].set(str(value2))

    def update_bc_defaults(self):
        """Update BC defaults based on case type."""
        # Safety check - ensure BC widgets exist
        if not hasattr(self, 'bc_widgets') or not self.bc_widgets:
            return
            
        case_num = self.icase.get()
        iinlet = int(self.iinlet_var.get().split(":")[0])

        for axis in ("x", "y", "z"):
            self._set_bc_entry(f"ifbc{axis}_u", BC.PERIODIC.value, BC.PERIODIC.value)
            self._set_bc_entry(f"ifbc{axis}_p", BC.PERIODIC.value, BC.PERIODIC.value)
            self._set_bc_entry(f"ifbc{axis}_T", BC.PERIODIC.value, BC.PERIODIC.value)
        
        # Y-direction defaults
        if case_num in WALL_BC_CASES:
            self._set_bc_entry("ifbcy_u", BC.DIRICHLET.value, BC.DIRICHLET.value)
            self._set_bc_entry("ifbcy_p", BC.NEUMANN.value, BC.NEUMANN.value)
            if self.ithermo.get():
                self._set_bc_entry(
                    "ifbcy_T",
                    BC.DIRICHLET.value,
                    BC.DIRICHLET.value,
                    self.Tini.get(),
                    self.Tini.get(),
                )
            else:
                self._set_bc_entry("ifbcy_T", BC.DIRICHLET.value, BC.DIRICHLET.value)
        elif case_num == Case.PIPE.value:
            self._set_bc_entry("ifbcy_u", BC.INTERIOR.value, BC.DIRICHLET.value)
            self._set_bc_entry("ifbcy_p", BC.INTERIOR.value, BC.NEUMANN.value)
            self._set_bc_entry(
                "ifbcy_T",
                BC.INTERIOR.value,
                BC.NEUMANN.value if self.ithermo.get() else BC.DIRICHLET.value,
            )
        elif case_num == Case.TGV3D.value:
            self.iinlet_var.set("1:Periodic")
        
        # X-direction defaults
        if case_num == Case.DUCT.value:
            self._set_bc_entry("ifbcx_u", BC.DIRICHLET.value, BC.DIRICHLET.value)
            self._set_bc_entry("ifbcx_p", BC.NEUMANN.value, BC.NEUMANN.value)
            self._set_bc_entry("ifbcx_T", BC.NEUMANN.value, BC.NEUMANN.value)

        if iinlet != BC.PERIODIC.value and case_num != Case.TGV3D.value:
            prefix = "ifbcz" if case_num == Case.DUCT.value else "ifbcx"
            inlet_temp = self.Tini.get() if self.ithermo.get() else "0.0"
            self._set_bc_entry(f"{prefix}_u", iinlet, BC.CONVOL.value)
            self._set_bc_entry(f"{prefix}_p", BC.NEUMANN.value, BC.NEUMANN.value)
            self._set_bc_entry(
                f"{prefix}_T", BC.DIRICHLET.value, BC.NEUMANN.value, inlet_temp, "0.0"
            )
            self.idriven.set("0:NONE")
        elif case_num == Case.DUCT.value:
            self.idriven.set("4:ZMFLUX")
        elif case_num != Case.TGV3D.value:
            self.idriven.set("1:XMFLUX")
        else:
            self.idriven.set("0:NONE")

        if hasattr(self, "is_read"):
            self.is_read.set(iinlet == BC.DATABS.value and case_num != Case.TGV3D.value)
            self.on_write_changed()

    def _create_bc_combo(self, parent, options, default, row, col):
        """Helper to create BC combobox."""
        var = tk.StringVar(value=f"{default}:Periodic" if ":" not in str(default) else str(default))
        widget = ttk.Combobox(parent, textvariable=var, state="readonly", width=15)
        widget["values"] = options
        widget.grid(row=row, column=col, padx=5, pady=5)
        return var, widget

    def create_scheme_tab(self):
        """Numerical scheme settings tab."""
        tab = ttk.Frame(self.notebook)
        self.notebook.add(tab, text="Scheme")

        self.dt, _ = self.create_labeled_input(tab, "Time step size", 0.00001, 0)
        
        accuracy_options = ["1:2nd CD", "2:4th CD", "3:4th CP", "4:6th CP"]
        self.iAccuracy, _ = self.create_labeled_input(
            tab, "Spatial accuracy", "1:2nd CD", 1, 0, "choice"
        )
        self.iAccuracy.set("1:2nd CD")
        # Need to get the widget to set values
        for child in tab.winfo_children():
            if isinstance(child, ttk.Combobox) and child.cget("textvariable") == str(self.iAccuracy):
                child["values"] = accuracy_options
                break

        self.sponge_length, self.sponge_length_w = self.create_labeled_input(
            tab, "Outlet sponge layer length", 0.0, 2
        )
        self.sponge_re, self.sponge_re_w = self.create_labeled_input(
            tab, "Sponge layer Reynolds number", 0.0, 3
        )

    def create_simcontrol_tab(self):
        """Simulation control settings tab."""
        tab = ttk.Frame(self.notebook)
        self.notebook.add(tab, text="SimControl")

        self.nIterFlowFirst, _ = self.create_labeled_input(
            tab, "First iteration for flow", 1, 0
        )
        self.nIterFlowLast, _ = self.create_labeled_input(
            tab, "Last iteration for flow", 1000000, 1
        )
        self.nIterThermoFirst, _ = self.create_labeled_input(
            tab, "First iteration for thermal", 1, 2
        )
        self.nIterThermoLast, _ = self.create_labeled_input(
            tab, "Last iteration for thermal", 1000000, 3
        )

    def create_io_tab(self):
        """I/O settings tab."""
        tab = ttk.Frame(self.notebook)
        self.notebook.add(tab, text="I/O")

        self.cpu_nfre, _ = self.create_labeled_input(
            tab, "CPU info print frequency", 1, 0
        )
        self.ckpt_nfre, _ = self.create_labeled_input(
            tab, "Checkpoint save frequency", 1000, 1
        )
        self.visu_nfre, _ = self.create_labeled_input(
            tab, "Visualization frequency", 500, 2
        )
        self.stat_istart, _ = self.create_labeled_input(
            tab, "Start statistics from iter", 1000, 3
        )

        visu_options = ["0:3-D only", "1:2-D planes only", "2:Both"]
        self.visu_idim, self.visu_idim_w = self.create_labeled_input(
            tab, "Visualization mode", "0:3-D only", 4, 0, "choice"
        )
        self.visu_idim_w["values"] = visu_options

        stat_options = [
            "0:None",
            "1:Mean flow",
            "2:+Reynolds stresses",
            "3:+Turbulent budgets",
        ]
        self.stat_level, self.stat_level_w = self.create_labeled_input(
            tab, "Statistics level", "3:+Turbulent budgets", 5, 0, "choice"
        )
        self.stat_level_w["values"] = stat_options

        self.stat_visu_nfre, _ = self.create_labeled_input(
            tab, "Visualized statistics frequency", 500, 6
        )

        stat_visu_mode_options = ["all", "tsp_only"]
        self.stat_visu_mode, self.stat_visu_mode_w = self.create_labeled_input(
            tab, "Visualized statistics mode", "all", 7, 0, "choice"
        )
        self.stat_visu_mode_w["values"] = stat_visu_mode_options

        output_policy_options = ["overwrite", "skip", "rename_existing"]
        self.existing_output_policy, self.existing_output_policy_w = self.create_labeled_input(
            tab, "Existing output policy", "overwrite", 8, 0, "choice"
        )
        self.existing_output_policy_w["values"] = output_policy_options

        layout_options = ["per_field", "bundled"]
        self.restart_data_layout, self.restart_data_layout_w = self.create_labeled_input(
            tab, "Restart/data output layout", "bundled", 9, 0, "choice"
        )
        self.restart_data_layout_w["values"] = layout_options

        self.reset_unit_massflux, self.reset_unit_massflux_w = self.create_labeled_input(
            tab, "Reset unit mass flux on restart?", 0, 10, 0, "bool"
        )

        precision_options = ["4:single", "8:double"]
        self.visu_precision, self.visu_precision_w = self.create_labeled_input(
            tab, "Visualisation precision", "4:single", 11, 0, "choice"
        )
        self.visu_precision_w["values"] = precision_options

        ttk.Label(tab, text="Write outlet plane data?").grid(
            row=12, column=0, sticky=tk.W, padx=5, pady=5
        )
        self.is_write_check = ttk.Checkbutton(
            tab, variable=self.is_write, command=self.on_write_changed
        )
        self.is_write_check.grid(row=12, column=1, sticky=tk.W, padx=5, pady=5)

        self.wrt_read_nfre1, self.wrt_read_nfre1_w = self.create_labeled_input(
            tab, "Plane data frequency", 1000, 13
        )
        self.wrt_read_nfre2, self.wrt_read_nfre2_w = self.create_labeled_input(
            tab, "Start saving from iter", 2001, 14
        )
        self.wrt_read_nfre3, self.wrt_read_nfre3_w = self.create_labeled_input(
            tab, "Stop saving at iter", 10000, 15
        )

        ttk.Label(tab, text="Read inlet plane data?").grid(
            row=16, column=0, sticky=tk.W, padx=5, pady=5
        )
        self.is_read = tk.BooleanVar(value=False)
        self.is_read_check = ttk.Checkbutton(
            tab, variable=self.is_read, command=self.on_write_changed
        )
        self.is_read_check.grid(row=16, column=1, sticky=tk.W, padx=5, pady=5)

        self.on_write_changed()

    def on_write_changed(self):
        """Handle write checkbox changes."""
        enabled = self.is_write.get() or self.is_read.get()
        self.set_enabled(self.wrt_read_nfre1_w, enabled)
        self.set_enabled(self.wrt_read_nfre2_w, enabled)
        self.set_enabled(self.wrt_read_nfre3_w, enabled)

    def create_probe_tab(self):
        """Probe settings tab."""
        tab = ttk.Frame(self.notebook)
        self.notebook.add(tab, text="Probe")

        ttk.Label(tab, text="Auto-generate 5 probe points?").grid(
            row=0, column=0, sticky=tk.W, padx=5, pady=5
        )
        self.is_auto_probe = tk.BooleanVar(value=True)
        ttk.Checkbutton(tab, variable=self.is_auto_probe).grid(
            row=0, column=1, sticky=tk.W, padx=5, pady=5
        )
        
        ttk.Label(tab, text="(Probes will be auto-generated at runtime)", 
                 font=("Arial", 9, "italic")).grid(
            row=1, column=0, columnspan=2, sticky=tk.W, padx=5, pady=5
        )

    def generate_ini(self):
        """Generate the INI file from GUI inputs."""
        try:
            config = CustomConfigParser()
            case_num = self.icase.get()

            # Process
            config["process"] = {
                "is_prerun": bool_to_string(self.is_prerun.get()),
                "is_postprocess": bool_to_string(self.is_postprocess.get()),
            }

            # Decomposition
            is_decomp = self.is_decomp.get()
            config["decomposition"] = {
                "nxdomain": 1,
                "p_row": 0 if is_decomp else int(self.p_row.get()),
                "p_col": 0 if is_decomp else int(self.p_col.get()),
            }

            # Domain
            config["domain"] = {
                "icase": case_num,
                "lxx": float(self.lxx.get()),
                "lyt": float(self.lyt.get()),
                "lyb": float(self.lyb.get()),
                "lzz": float(self.lzz.get()),
            }

            # Flow - determine initfl based on restart and case
            if self.is_restart.get():
                initfl = Init.RESTART.value
                irestartfrom = int(self.irestartfrom.get())
            else:
                if case_num == Case.TGV3D.value:
                    initfl = Init.FUNCTION.value
                else:
                    initfl = int(self.initfl.get().split(":")[0])
                irestartfrom = 0

            config["flow"] = {
                "initfl": initfl,
                "irestartfrom": irestartfrom,
                "veloinit": f"{float(self.velo1.get())},{float(self.velo2.get())},{float(self.velo3.get())}",
                "noiselevel": float(self.noiselevel.get()),
                "reni": int(self.reni.get()),
                "nreni": int(self.nreni.get()),
                "ren": int(self.ren.get()),
            }

            # Thermo
            if self.ithermo.get():
                use_qw_ramp = self.use_qw_ramp.get()
                config["thermo"] = {
                    "ithermo": bool_to_string(self.ithermo.get()),
                    "icht": bool_to_string(self.icht.get()),
                    "igravity": int(self.igravity.get().split(":")[0]),
                    "ifluid": int(self.ifluid.get().split(":")[0]),
                    "ref_l0": float(self.refl0.get()),
                    "ref_T0": float(self.refT0.get()),
                    "inittm": int(self.inittm.get().split(":")[0]),
                    "irestartfrom": 0,
                    "Tini": float(self.Tini.get()),
                    "inout_buffer": f"{float(self.buffer_inlet.get())},{float(self.buffer_outlet.get())}",
                    "qw_ramp": (
                        f"{bool_to_string(use_qw_ramp)},"
                        f"{int(self.qw_ramp_start.get())},{int(self.qw_ramp_end.get())}"
                    ),
                }

            # MHD
            if self.imhd.get():
                is_stuart = self.mhd_type.get().startswith("1")
                config["mhd"] = {
                    "imhd_xdom": bool_to_string(self.imhd.get()),
                    "NStuart": f"{bool_to_string(is_stuart)},{float(self.NS.get())}",
                    "NHartmn": f"{bool_to_string(not is_stuart)},{float(self.NH.get())}",
                    "B_static": f"{float(self.b1.get())},{float(self.b2.get())},{float(self.b3.get())}",
                }

            # LES
            config["les"] = {"LESmode": "DNS"}

            # Mesh
            istret_val = int(self.istret.get().split(":")[0])
            config["mesh"] = {
                "ncx": int(self.ncx.get()),
                "ncy": int(self.ncy.get()),
                "ncz": int(self.ncz.get()),
                "istret": istret_val,
                "rstret": f"{int(self.rstret1.get())},{float(self.rstret2.get())}"
                if istret_val != 0
                else "0,0.0",
            }

            # BC
            bc_dict = {}
            for key in self.bc_widgets:
                if "_1" in key or "_2" in key or "_v1" in key or "_v2" in key:
                    base_key = key.rsplit("_", 1)[0]
                    if base_key not in bc_dict:
                        # Build BC string for this boundary
                        bc1 = self.bc_widgets[f"{base_key}_1"].get().split(":")[0]
                        bc2 = self.bc_widgets[f"{base_key}_2"].get().split(":")[0]
                        v1 = self.bc_widgets[f"{base_key}_v1"].get()
                        v2 = self.bc_widgets[f"{base_key}_v2"].get()
                        bc_dict[base_key] = f"{bc1},{bc2},{v1},{v2}"

            inlet_bc = int(self.iinlet_var.get().split(":")[0])
            if inlet_bc != BC.PERIODIC.value and case_num != Case.TGV3D.value:
                streamwise_prefix = "ifbcz" if case_num == Case.DUCT.value else "ifbcx"
                inlet_temp = self.Tini.get() if self.ithermo.get() else "0.0"
                bc_dict[f"{streamwise_prefix}_u"] = f"{inlet_bc},7,0.0,0.0"
                bc_dict[f"{streamwise_prefix}_p"] = "5,5,0.0,0.0"
                bc_dict[f"{streamwise_prefix}_T"] = f"4,5,{inlet_temp},0.0"

            config["bc"] = {
                "ifbcx_u": bc_dict.get("ifbcx_u", "1,1,0.0,0.0"),
                "ifbcx_v": bc_dict.get("ifbcx_u", "1,1,0.0,0.0"),
                "ifbcx_w": bc_dict.get("ifbcx_u", "1,1,0.0,0.0"),
                "ifbcx_p": bc_dict.get("ifbcx_p", "1,1,0.0,0.0"),
                "ifbcx_t": bc_dict.get("ifbcx_T", "1,1,0.0,0.0"),
                "ifbcy_u": bc_dict.get("ifbcy_u", "4,4,0.0,0.0"),
                "ifbcy_v": bc_dict.get("ifbcy_u", "4,4,0.0,0.0"),
                "ifbcy_w": bc_dict.get("ifbcy_u", "4,4,0.0,0.0"),
                "ifbcy_p": bc_dict.get("ifbcy_p", "5,5,0.0,0.0"),
                "ifbcy_t": bc_dict.get("ifbcy_T", "1,1,0.0,0.0"),
                "ifbcz_u": bc_dict.get("ifbcz_u", "1,1,0.0,0.0"),
                "ifbcz_v": bc_dict.get("ifbcz_u", "1,1,0.0,0.0"),
                "ifbcz_w": bc_dict.get("ifbcz_u", "1,1,0.0,0.0"),
                "ifbcz_p": bc_dict.get("ifbcz_p", "1,1,0.0,0.0"),
                "ifbcz_t": bc_dict.get("ifbcz_T", "1,1,0.0,0.0"),
                "idriven": 0
                if inlet_bc != BC.PERIODIC.value and case_num != Case.TGV3D.value
                else int(self.idriven.get().split(":")[0]),
                "drivenfc": float(self.drivenCf.get()),
            }

            # Scheme
            accuracy_map = {"1": "cd2", "2": "cd4", "3": "cp4", "4": "cp6"}
            config["scheme"] = {
                "dt": float(self.dt.get()),
                "itimescheme": "rk3",
                "iaccuracy": accuracy_map[self.iAccuracy.get().split(":")[0]],
                "iviscous": "explicit",
                "out_sponge_L_Re": f"{float(self.sponge_length.get())},{float(self.sponge_re.get())}",
            }

            # Simulation Control
            config["simcontrol"] = {
                "niterflowfirst": int(self.nIterFlowFirst.get()),
                "niterflowlast": int(self.nIterFlowLast.get()),
                "niterthermofirst": int(self.nIterThermoFirst.get()) if self.ithermo.get() else 0,
                "niterthermolast": int(self.nIterThermoLast.get()) if self.ithermo.get() else 0,
            }

            # I/O
            is_read = 1 if self.is_read.get() or inlet_bc == BC.DATABS.value else 0
            is_write = 1 if self.is_write.get() else 0

            if is_write == 0 and is_read == 0:
                wrt_read_nfre = "0,0,0"
            else:
                wrt_read_nfre = f"{int(self.wrt_read_nfre1.get())},{int(self.wrt_read_nfre2.get())},{int(self.wrt_read_nfre3.get())}"

            config["io"] = {
                "cpu_nfre": int(self.cpu_nfre.get()),
                "ckpt_nfre": int(self.ckpt_nfre.get()),
                "visu_idim": int(self.visu_idim.get().split(":")[0]),
                "visu_nfre": int(self.visu_nfre.get()),
                "visu_nskip": DEFAULT_VISU_SKIP,
                "stat_istart": int(self.stat_istart.get()),
                "stat_level": int(self.stat_level.get().split(":")[0]),
                "stat_nskip": DEFAULT_STAT_SKIP,
                "stat_visu_nfre": int(self.stat_visu_nfre.get()),
                "stat_visu_mode": self.stat_visu_mode.get(),
                "is_record_xoutlet_read_xinlet": f"{bool_to_string(is_write)},{bool_to_string(is_read)}",
                "ndbfre_ndbstart_ndbend": wrt_read_nfre,
                "existing_output_policy": self.existing_output_policy.get(),
                "restart_data_layout": self.restart_data_layout.get(),
                "restart_data_layout_read": self.restart_data_layout.get(),
                "restart_data_layout_write": self.restart_data_layout.get(),
                "reset_unit_massflux": bool_to_string(self.reset_unit_massflux.get()),
                "visu_precision": int(self.visu_precision.get().split(":")[0]),
            }

            # Probe - auto-generate 5 points
            probe_settings = self._generate_probe_settings()
            config["probe"] = probe_settings

            # Save file
            with open(DEFAULT_FILENAME, "w") as configfile:
                config.write(configfile)

            messagebox.showinfo("Success", f"Configuration saved to {DEFAULT_FILENAME}")

        except Exception as e:
            messagebox.showerror("Error", f"Failed to generate INI file:\n{str(e)}")

    def _generate_probe_settings(self):
        """Generate automatic 5 probe points."""
        npp = 5
        lxx = float(self.lxx.get())
        lzz = float(self.lzz.get())
        lyt = float(self.lyt.get())
        lyb = float(self.lyb.get())

        lxp = [lxx / 2.0] * npp
        lzp = [lzz / 2.0] * npp
        lyp = [lyb + (lyt - lyb) * (i + 1) / (npp + 1) for i in range(npp)]

        result = {"npp": npp}
        for i in range(npp):
            result[f"pt{i + 1}"] = f"{lxp[i]},{lyp[i]},{lzp[i]}"

        return result


if __name__ == "__main__":
    ensure_template_available()
    if TKINTER_IMPORT_ERROR is not None:
        raise SystemExit(
            "Tkinter is required to run autoinput_gui.py. "
            f"Original import error: {TKINTER_IMPORT_ERROR}"
        )
    root = tk.Tk()
    gui = CHAPSimGUI(root)
    root.mainloop()
