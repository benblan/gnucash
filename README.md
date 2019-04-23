# gnucash

## Custom report budget

**How to install the report**
For more details, you can chech the GNU Cash wiki page.

1/ Copy the budget-custom.scm file in a directory in your user account (for example).
2/ For GnuCash v3.x edit GNC_CONFIG_HOME/config-user.scm to add the following line:
(load (gnc-build-userdata-path "path-to/budget-custom.scm"))
3/ Copy the budget-custom.eguile.scm in the GNC_DATA_HOME directory