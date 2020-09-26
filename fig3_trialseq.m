%% load a typical data from UML procedure and visualize the sequences
%% and related parameter etimates. 
load('sub_trialseq.mat');
%%
figure;
for i=1:3
    uml = u{i}; % get individual UML data
    Mba = exp(squeeze(max(uml.p,[],3))'); % squeeze alpha and beta
    subplot(1,3,i);
    xidx = 50:175; 
    yidx = 55:150;
    h=contourf(uml.beta(xidx),uml.alpha(yidx),Mba(yidx,xidx),'LineStyle','none');
    xlabel('\beta','FontSize',10);
    if i==1 
        ylabel('\alpha', 'FontSize',10);
    end
    title(['Set ', num2str(i)]);
end


%% plot converge of alpha
figure;
hold on;
for i=1:3
    uml = u{i}; % get individual UML data
    plot(uml.phi(:,1));
end
legend('Set 1','Set 2', 'Set 3')
xlabel('Trial No.');
ylabel('Threshold (ms)');



