$(document).on('shiny:connected', function() {
  
  function createOption(opt) {
    return `<option value="${opt}">${opt}</option>`
  }
  
  /*
  This handler is necessary to handle the roles with `{shinymanger}`
  The administrative tools for `{shinymanager}` are text inputs. The application
  needs to restrict these to the list of provided roles. Furthermore, the roles
  are restricted based on whether or not the user is an admin.
  */
  Shiny.addCustomMessageHandler('roles', function(role_lst) {
    let roleEl = $(`#${role_lst.id}-role`);
    let roleClass = roleEl.attr('class');
    let roleValue = roleEl.val();
    // Create string of options. If admin, the admin roles. Else, other roles.
    let role_opts = ($(`#${role_lst.id}-admin`).is(':checked')) ? `${role_lst.role_opts.admin.map(createOption).join("")}` : `${role_lst.role_opts.nonadmin.map(createOption).join("")}`;
    // Replace text input with select input with appropriate options
    roleEl.replaceWith(`<select id="${role_lst.id}-role" class="${roleClass}">${role_opts}</select>`)
    if (roleValue != '')
     $(`#${role_lst.id}-role`).val(roleValue);
    // Reconnect the appropriate shiny reactive with the select input
    $(`#${role_lst.id}-role`).change(function() {{
     Shiny.setInputValue(`${role_lst.id}-role`, $(this).val());
    }})
    // Create observe to change options is user admin role changed.
    $(`#${role_lst.id}-admin`).change(function() {{
    if ($(this).is(':checked')) {{
      $(`#${role_lst.id}-role`).html(`${role_lst.role_opts.admin.map(createOption).join("")}`);
    }} else {{
      $(`#${role_lst.id}-role`).html(`${role_lst.role_opts.nonadmin.map(createOption).join("")}`);
    }}
    }})
  })

})