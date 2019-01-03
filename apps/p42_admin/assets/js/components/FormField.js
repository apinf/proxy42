import React, { Component } from 'react';

import { Field as ReduxFormField } from 'redux-form';
import { FormField as GrommetFormField} from 'grommet';

// Grommet's components expect value, onchange etc as props directly.
// Redux form supplies them under input key.  This wrapper does the
// translation. And allows specifying label directly and wraps input
// component grommets formfield to render label properly.

const wrapGrommetInput = (GrommetInput) => (field) => {
  return <GrommetFormField
    label={field.label}
    htmlFor={field.id}>
    <GrommetInput
      value={field.input.value}
      onChange={field.input.onChange}
      onSelect={field.input.onSelect}
      id={field.id}
      placeholder={field.placeholder}
      {...field}
    />
  </GrommetFormField>
}

export default class FormField extends Component {
  render(){
    const {component} = this.props;
    const props = {...this.props,
      component: wrapGrommetInput(component),
    }
    return <ReduxFormField {...props} />
  }
}
