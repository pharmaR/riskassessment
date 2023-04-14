$(document).on('shiny:connected', function() {
  
  function createOption(opt) {
    return `<option value="${opt}">${opt}</option>`
  }
  
  Shiny.addCustomMessageHandler('roles', function(role_lst) {
    let roleEl = $(`#${role_lst.id}-role`);
    let roleClass = roleEl.attr('class');
    let roleValue = roleEl.val();
    let role_opts = ($(`#${role_lst.id}-admin`).is(':checked')) ? `${role_lst.role_opts.admin.map(createOption).join("")}` : `${role_lst.role_opts.nonadmin.map(createOption).join("")}`;
    roleEl.replaceWith(`<select id="${role_lst.id}-role" class="${roleClass}">${role_opts}</select>`)
    if (roleValue != '')
     $(`#${role_lst.id}-role`).val(roleValue);
    $(`#${role_lst.id}-role`).change(function() {{
     Shiny.setInputValue(`${role_lst.id}-role`, $(this).val());
    }})
    $(`#${role_lst.id}-admin`).change(function() {{
    if ($(this).is(':checked')) {{
      $(`#${role_lst.id}-role`).html(`${role_lst.role_opts.admin.map(createOption).join("")}`);
    }} else {{
      $(`#${role_lst.id}-role`).html(`${role_lst.role_opts.nonadmin.map(createOption).join("")}`);
    }}
    }})
  })

})