import React from 'react';
import ReactDOM from 'react-dom';
import SmsForm from './SmsForm';
import AvarcomTasks from './AvarcomTasks';
import DiagTreeEditor from './DiagTree/Editor';
import DiagTreeShow from './DiagTree/Show';
import './index.css';

window.React = React;
window.ReactDOM = ReactDOM;
window.CarmaComponents = {
  SmsForm: SmsForm,
  AvarcomTasks: AvarcomTasks,
  DiagTree: {
    Editor: DiagTreeEditor,
    Show: DiagTreeShow,
  }
};
